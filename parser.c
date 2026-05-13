#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "utils.h"
#include "at.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

Symbol *owner = NULL;

void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

bool consume(int code){
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
		}
	return false;
	}

bool unit();
bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
bool expr(Ret *r);	
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprOrPrim(Ret *r);
bool exprAnd(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEq(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRel(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAdd(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMul(Ret *r);
bool exprMulPrim(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPostfixPrim(Ret *r);
bool exprPrimary(Ret *r);

// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
	t->n=-1;		// not an array by default
	if(consume(TYPE_INT)){ t->tb=TB_INT; return true;}
	if(consume(TYPE_DOUBLE)){ t->tb=TB_DOUBLE; return true;}
	if(consume(TYPE_CHAR)){ t->tb=TB_CHAR; return true;}
	if(consume(STRUCT))
	{
		if(consume(ID)){
			Token *tkName = consumedTk;
			t->tb=TB_STRUCT;
			t->s=findSymbol(tkName->text);
			if(!t->s) tkerr("undefined struct '%s'", tkName->text);
			return true;
		}else tkerr("identifier expected after struct");
	}
	return false;
}

bool structDef(){
    Token *start=iTk;
    if(consume(STRUCT)){
        if(consume(ID)){
            Token *tkName=consumedTk;
           if(consume(LACC)){
				Symbol *s=findSymbolInDomain(symTable,tkName->text);
				if(s) tkerr("symbol '%s' already defined in this scope", tkName->text);
				s=addSymbolToDomain(symTable, newSymbol(tkName->text,SK_STRUCT));
				s->type.tb=TB_STRUCT;
				s->type.s=s;
				s->type.n=-1;		// not an array
				pushDomain();
				owner=s;
                while(varDef()){}
                if(consume(RACC)){
                    if(consume(SEMICOLON)){
						owner=NULL;
						dropDomain();
                        return true;
                    } else tkerr("; expected after struct definition");
                } else tkerr("} expected to close struct definition");
                
            } else {
                    iTk=start;
                    return false; 
            }
   	 	} else tkerr("identifier expected after struct");
	}
    iTk=start;
    return false;
}
bool varDef(){
	Type t;
    Token *start=iTk;
    if(typeBase(&t)){
        if(consume(ID)){
			Token *tkName=consumedTk;
            if(arrayDecl(&t)){
				if(t.n==0) tkerr("array size expected for array declaration");
			}
            if(consume(ASSIGN)){
				Ret rExpr;
                if(!expr(&rExpr)) tkerr("expression expected after '=' in variable initialization");
				if(!convTo(&rExpr.type,&t)) tkerr("cannot convert initializer to variable type");
            }
            if(consume(SEMICOLON)){
				Symbol *var=findSymbolInDomain(symTable,tkName->text);
				if(var) tkerr("symbol '%s' already defined in this scope", tkName->text);
				var=newSymbol(tkName->text,SK_VAR);
				var->type=t;	
				var->owner=owner;
				addSymbolToDomain(symTable,var);
				if(owner){
					switch(owner->kind){
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers,dupSymbol(var));
							break;
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals,dupSymbol(var));
							break;
						default: break;
						}
				} else{
					var->varMem=safeAlloc(typeSize(&t));
				}
				return true;
            } else tkerr("; expected after variable definition");    
        } else {
            tkerr("missing variable name (or function name) after data type, OR missing '{' for struct definition");
        }
    }
    iTk=start;
    return false;
}
bool arrayDecl(Type *t){
	Token *start=iTk;
	if(consume(LBRACKET)){
		if(consume(INT)){
			Token *tkSize=consumedTk;
			t->n=tkSize->i;
		} else {t->n=0;}
		if(consume(RBRACKET)){
				return true;
			} else tkerr("] expected after array size");
		}
	iTk=start;
	return false;
}

bool fnDef(){
	Type t;
	Token *start=iTk;
	if(typeBase(&t) || consume(VOID)){
		if(consumedTk->code==VOID){
			t.tb=TB_VOID;
		}
		if(consume(ID)){
			Token *tkName=consumedTk;
			if(consume(LPAR)){
				Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				if(fn) tkerr("symbol '%s' already defined in this scope", tkName->text);
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain();
				if(fnParam()){}
				while(consume(COMMA)){
					if(!fnParam())tkerr("missing or invalid parameter after comma");
				}
				if(consume(RPAR)){
					if(stmCompound(false)){
						dropDomain();
						owner=NULL;
						return true;
					} else tkerr("function body expected after function declaration");
				} else tkerr(") expected to close parameter list");
			} else { iTk=start; return false; }
		} else tkerr("identifier expected after type");
	}
	iTk=start;
	return false;
}

bool fnParam(){
	Type t;
	Token *start=iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName=consumedTk;
			if(arrayDecl(&t)){
				t.n=0;		// array parameters are treated as arrays without specified dimension
			}
			Symbol *param=findSymbolInDomain(symTable,tkName->text);
			if(param) tkerr("symbol '%s' already defined in this scope", tkName->text);
			param=newSymbol(tkName->text,SK_PARAM);
			param->type=t;
			param->owner=owner;
			param->paramIdx=symbolsLen(owner->fn.params);
			addSymbolToDomain(symTable,param);
			addSymbolToList(&owner->fn.params,dupSymbol(param));
			return true;
		} else tkerr("identifier expected after type in parameter");
	}
	iTk=start;
	return false;
}

bool stm(){
	Token *start=iTk;
    Ret rCond,rExpr;
	if(stmCompound(true)) return true;

	if(consume(IF)){
		if(consume(LPAR)){
			if(expr(&rCond)){
                if(!canBeScalar(&rCond)) tkerr("the if condition must be a scalar value");
				if(consume(RPAR)){
					if(stm()){
						if(consume(ELSE)){
							if(!stm()){
								tkerr("statement expected after else");
							}
						} return true;
					} else tkerr("statement expected after if condition");
				} else tkerr(") expected to close if condition");
			}  else { iTk=start; return false; } //else  tkerr("expression expected in if condition");
		} else tkerr("( expected to open if condition");
	}
	else if(consume(WHILE)){
		if(consume(LPAR)){
			if(expr(&rCond)){
                if(!canBeScalar(&rCond)) tkerr("the while condition must be a scalar value");
				if(consume(RPAR)){
					if(stm()){
						return true;
					} else tkerr("statement expected after while condition");
				} else tkerr(") expected to close while condition");
			}  else { iTk=start; return false; } //else tkerr("expression expected in while condition");
		} else tkerr("( expected to open while condition");
	}
	else if(consume(RETURN)){
		if(expr(&rExpr)){
            if(owner->type.tb==TB_VOID) tkerr("cannot return a value from a void function");
            if(!canBeScalar(&rExpr)) tkerr("the return value must be a scalar value");
            if(!convTo(&rExpr.type,&owner->type)) tkerr("cannot convert return value to function's return type");
        }else{
            if(owner->type.tb!=TB_VOID) tkerr("a return value is expected for non-void functions");
        }
		if(consume(SEMICOLON)){
				return true;
			} else tkerr("; expected after return value");
	} 

	Token *exprStart=iTk;
	if(expr(&rExpr)){
		if(consume(SEMICOLON)){return true;} 
		tkerr("; expected after expression statement");
	}

	iTk=start;
	return false;
}

bool stmCompound(bool newDomain){
	Token *start=iTk;
	if(consume(LACC)){
		if(newDomain) pushDomain();
		while(varDef() || stm()){}
		if(consume(RACC)){
			if(newDomain) dropDomain();
			return true;
		} else tkerr("} expected to close compound statement");
	} 
	iTk=start;
	return false;
}

// unit: ( structDef | fnDef | varDef )* END
bool unit(){
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break;
		}

	if(iTk->code == RACC){
        tkerr("Acolada inchisa '}' in plus (posibil ai uitat sa deschizi un '{' la un if/while)");
    }

	if(consume(END)){
		return true;
		}
	return false;
	}


bool expr(Ret *r){
	return exprAssign(r);
}

bool exprAssign(Ret *r){
	Token *start=iTk;
    Ret rDst;
	if(exprUnary(&rDst)){
		if(consume(ASSIGN)){
			if(exprAssign(r)){
                if(!rDst.lval) tkerr("the assign destination must be a left-value");
                if(rDst.ct) tkerr("cannot assign to a constant value");
                if(!canBeScalar(&rDst)) tkerr("the assign destination must be scalar");
                if(!canBeScalar(r)) tkerr("the assign source must be scalar");
                if(!convTo(&r->type,&rDst.type)) tkerr("the assign source cannot be converted to destination");
                r->lval=false;
                r->ct=true;
				return true;
			} else tkerr("expression expected after =");
		} 
	}
	iTk = start;
    return exprOr(r);
}
// exprOr: exprAnd exprOrPrim
bool exprOr(Ret *r){
	Token *start=iTk;
	if(exprAnd(r)){
		if(exprOrPrim(r)){
			return true;
		} else tkerr("syntax error in || expression");
	}
	iTk=start;
	return false;
}
// exprOrPrim: OR exprAnd exprOrPrim | epsilon
bool exprOrPrim(Ret *r){
	if(consume(OR)){
        Ret right;
		if(exprAnd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for ||");
            *r=(Ret){{TB_INT,NULL,-1},false,true};	
			if(exprOrPrim(r)){
				return true;
			} else tkerr("syntax error in || expression");
		} else tkerr("expression expected after ||");
	}
	return true;
}
// exprAnd: exprEq exprAndPrim
bool exprAnd(Ret *r){
	Token *start=iTk;
	if(exprEq(r)){
		if(exprAndPrim(r)){
			return true;
		} else tkerr("syntax error in && expression");
	}
	iTk=start;
	return false;
}
// exprAndPrim: AND exprEq exprAndPrim | epsilon
bool exprAndPrim(Ret *r){
	if(consume(AND)){
        Ret right;
		if(exprEq(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for &&");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprAndPrim(r)){
				return true;
			} else tkerr("syntax error in && expression");
		} else tkerr("expression expected after &&");
	}
	return true;
}
// exprEq: exprRel exprEqPrim
bool exprEq(Ret *r){
	Token *start=iTk;
	if(exprRel(r)){
		if(exprEqPrim(r)){
			return true;
		} else tkerr("syntax error in == or != expression");
	}
	iTk=start;
	return false;
}
// exprEqPrim: (EQUAL|NEQUAL) exprRel exprEqPrim | epsilon
bool exprEqPrim(Ret *r){
	if(consume(EQUAL)){
        Ret right;
		if(exprRel(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for ==");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprEqPrim(r)){
				return true;
			} else tkerr("syntax error in == expression");
		} else tkerr("expression expected after ==");
	}
	if(consume(NOTEQ)){
        Ret right;
		if(exprRel(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for !=");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprEqPrim(r)){
				return true;
			} else tkerr("syntax error in != expression");
		} else tkerr("expression expected after !=");
	}
	return true;
}
// exprRel: exprAdd exprRelPrim
bool exprRel(Ret *r){
	Token *start=iTk;
	if(exprAdd(r)){
		if(exprRelPrim(r)){
			return true;
		} else tkerr("syntax error in rel expression");
	}
	iTk=start;
	return false;
}
// exprRelPrim: (LESS|GREATER|LESSEQ|GREATEREQ) exprAdd exprRelPrim | epsilon
bool exprRelPrim(Ret *r){
	if(consume(LESS)){
        Ret right;
		if(exprAdd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for <");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprRelPrim(r)){
				return true;
			} else tkerr("syntax error in < expression");
		} else tkerr("expression expected after < operator");
	}
	if(consume(GREATER)){
        Ret right;
		if(exprAdd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for >");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprRelPrim(r)){
				return true;
			} else tkerr("syntax error in > expression");
		} else tkerr("expression expected after > operator");
	}
	if(consume(LESSEQ)){
        Ret right;
		if(exprAdd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for <=");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprRelPrim(r)){
				return true;
			} else tkerr("syntax error in <= expression");
		} else tkerr("expression expected after <= operator");
	}
	if(consume(GREATEREQ)){
        Ret right;
		if(exprAdd(&right)){
            Type tDst;
            if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for >=");
            *r=(Ret){{TB_INT,NULL,-1},false,true};
			if(exprRelPrim(r)){
				return true;
			} else tkerr("syntax error in >= expression");
		} else tkerr("expression expected after >= operator");
	}
	return true;
}
// exprAdd: exprMul exprAddPrim
bool exprAdd(Ret *r){
	Token *start=iTk;
	if(exprMul(r)){
		if(exprAddPrim(r)){
			return true;
		} else tkerr("syntax error in add expression");
	}
	iTk=start;
	return false;
}
// exprAddPrim: (ADD|SUB) exprMul exprAddPrim | epsilon
bool exprAddPrim(Ret *r){
	if(consume(ADD)){
		Ret right;
		if(exprMul(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for +");
			*r=(Ret){tDst,false,true};
			if(exprAddPrim(r)){
				return true;
			} else tkerr("syntax error in add(+) expression");
		} else tkerr("expression expected after + ");
	}
	if(consume(SUB)){
		Ret right;
		if(exprMul(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for -");
			*r=(Ret){tDst,false,true};
			if(exprAddPrim(r)){
				return true;
			} else tkerr("syntax error in sub(-) expression");
		} else tkerr("expression expected after -");
	}
	return true;
}
// exprMul: exprCast exprMulPrim
bool exprMul(Ret *r){		
	Token *start=iTk;
	if(exprCast(r)){
		if(exprMulPrim(r)){
			return true;
		} else tkerr("syntax error in mul expression");
	}
	iTk=start;
	return false;
}
// exprMulPrim: (MUL|DIV) exprCast exprMulPrim | epsilon
bool exprMulPrim(Ret *r){
	if(consume(MUL)){
		Ret right;
		if(exprCast(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for *");
			*r=(Ret){tDst,false,true};
			if(exprMulPrim(r)){
				return true;
			} else tkerr("syntax error in mul(*) expression");
		} else tkerr("expression expected after * ");
	}
	if(consume(DIV)){
		Ret right;
		if(exprCast(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type,&right.type,&tDst)) tkerr("invalid operand type for /");
			*r=(Ret){tDst,false,true};
			if(exprMulPrim(r)){
				return true;
			} else tkerr("syntax error in div(/) expression");
		} else tkerr("expression expected after /");
	}
	return true;
}
//exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(Ret *r){
	Token *start=iTk;
	if(consume(LPAR)){
		Type t;
		Ret op;
		if(typeBase(&t)){
			if(arrayDecl(&t)){}
			if(consume(RPAR)){
				if(exprCast(&op)){
					if(t.tb==TB_STRUCT) tkerr("cannot convert to struct type");
					if(op.type.tb==TB_STRUCT) tkerr("cannot convert from struct type");
					if(op.type.n>=0&& t.n<0) tkerr("an array can be converted only to another array");
					if(op.type.n<0&&t.n>=0) tkerr("a scalar can be converted only to another scalar");
					*r=(Ret){t,false,true};
					return true;
				} else tkerr("expression expected after cast");
			} else tkerr(") expected to close cast");
		} else tkerr("type expected in cast");
	}
	iTk=start;
	return exprUnary(r);
}
//exprUnary: (SUB|NOT) exprUnary | exprPostfix
bool exprUnary(Ret *r){
	Token *start=iTk;
	if(consume(SUB)){
		if(exprUnary(r)){
			if(!canBeScalar(r)) tkerr("- operator must have a scalar operand");
			r->lval=false;
			r->ct=true;
			return true;
		} else tkerr("expression expected after - operator");
	}
	if(consume(NOT)){
		if(exprUnary(r)){
			if(!canBeScalar(r)) tkerr("! operator must have a scalar operand");
			r->lval=false;
			r->ct=true;
			return true;
		} else tkerr("expression expected after ! operator");
	}
	iTk=start;
	return exprPostfix(r);
}
// exprPostfix: exprPrimary exprPostfixPrim
bool exprPostfix(Ret *r) {
    Token *start = iTk;
    if (exprPrimary(r)) {
        if (exprPostfixPrim(r)) {
            return true;
        }else tkerr("syntax error in postfix expression");
    }
    iTk = start;
    return false;
}
// exprPostfixPrim: ( LBRACKET expr RBRACKET | DOT ID ) exprPostfixPrim | epsilon
bool exprPostfixPrim(Ret *r) {
	if (consume(LBRACKET)) {
		Ret idx;
		if (expr(&idx)) {
			if (consume(RBRACKET)) {
				if(r->type.n<0) tkerr("cannot index a non-array value");
				Type tInt={TB_INT,NULL,-1};
				if(!convTo(&idx.type,&tInt)) tkerr("the index is not convertible to int");
				r->type.n=-1;		
				r->lval=true;
				r->ct=false;
				if (exprPostfixPrim(r)) {
					return true;
				} else tkerr("syntax error in postfix expression");
			} else tkerr("] expected after array index");
		} else tkerr("expression expected in array index");
	} else if (consume(DOT)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (r->type.tb != TB_STRUCT) tkerr("a field can only be selected from a struct");
			Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
			if (!s) tkerr("struct '%s' has no field named '%s'", r->type.s->name, tkName->text);
			*r=(Ret){s->type,true,s->type.n>=0};
			if (exprPostfixPrim(r)) {
				return true;
			} else tkerr("syntax error in postfix expression");
		} else tkerr("identifier expected after .");
	}
	return true;
}
// exprPrimary:  ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?| INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(Ret *r){
	Token *start=iTk;
	if(consume(ID)){
		Token *tkName=consumedTk;
		Symbol *s=findSymbol(tkName->text);
		if(!s) tkerr("undefined id: '%s'", tkName->text);
		if(consume(LPAR)){
			if(s->kind!=SK_FN) tkerr("only functions can be called");
			Ret rArg;
			Symbol *param=s->fn.params;
			if(expr(&rArg)){
				if(!param) tkerr("too many arguments in function call to '%s'", s->name);
				if(!convTo(&rArg.type,&param->type)) tkerr("argument type mismatch in function call to '%s'", s->name);
				param=param->next;
				while(consume(COMMA)){
					if(expr(&rArg)){
						if(!param) tkerr("too many arguments in function call to '%s'", s->name);
						if(!convTo(&rArg.type,&param->type)) tkerr("argument type mismatch in function call to '%s'", s->name);
						param=param->next;
					} else tkerr("expression expected after comma in argument list");
				}
			}
			if(consume(RPAR)){
				if(param) tkerr("too few arguments in function call to '%s'", s->name);
				*r=(Ret){s->type,false,true};
				return true;
			} else tkerr(") expected to close argument list");
		} else {
			if(s->kind==SK_FN) tkerr("a function can only be called");
			*r=(Ret){s->type,true,s->type.n>=0};
			return true;
		}
	}
	else if(consume(INT)){*r=(Ret){{TB_INT,NULL,-1},false,true}; return true;}
	else if(consume(DOUBLE)){*r=(Ret){{TB_DOUBLE,NULL,-1},false,true}; return true;}
	else if(consume(CHAR)){*r=(Ret){{TB_CHAR,NULL,-1},false,true}; return true;}
	else if(consume(STRING)){*r=(Ret){{TB_CHAR,NULL,0},false,true}; return true;}
	else if(consume(LPAR)){
		if(expr(r)){
			if(consume(RPAR)){
				return true;
			} else tkerr(") expected to close expression");
		} else tkerr("expression expected after (");
	}

    if (iTk->code==ADD || iTk->code == MUL || iTk->code == DIV || iTk->code == EQUAL || iTk->code==NOTEQ || iTk->code == ASSIGN || iTk->code == LESS || iTk->code == LESSEQ || iTk->code == GREATER || iTk->code == GREATEREQ || iTk->code == AND || iTk->code == OR) {
        tkerr("lipseste operandul din stanga pentru operator");
    }
	iTk=start;
	return false;
}

void parse(Token *tokens){
	iTk=tokens;
	pushDomain();		// the global domain

	if(!unit())tkerr("syntax error");

	showDomain(symTable,"global");
	dropDomain();
}
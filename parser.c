#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "utils.h"

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
bool expr();	
bool exprAssing();
bool exprOr();
bool exprOrPrim();
bool exprAnd();
bool exprAndPrim();
bool exprEq();
bool exprEqPrim();
bool exprRel();
bool exprRelPrim();
bool exprAdd();
bool exprAddPrim();
bool exprMul();
bool exprMulPrim();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPostfixPrim();
bool exprPrimary();

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
                if(!expr()) tkerr("expression expected after '=' in variable initialization");
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
	if(stmCompound(true)) return true;

	if(consume(IF)){
		if(consume(LPAR)){
			if(expr()){
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
			if(expr()){
				if(consume(RPAR)){
					if(stm()){
						return true;
					} else tkerr("statement expected after while condition");
				} else tkerr(") expected to close while condition");
			}  else { iTk=start; return false; } //else tkerr("expression expected in while condition");
		} else tkerr("( expected to open while condition");
	}
	else if(consume(RETURN)){
		if(expr()){}
		if(consume(SEMICOLON)){
				return true;
			} else tkerr("; expected after return value");
	} 

	Token *exprStart=iTk;
	if(expr()){
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


bool expr(){
	return exprAssing();
}

bool exprAssing(){
	Token *start=iTk;

	if(exprUnary()){
		if(consume(ASSIGN)){
			if(exprAssing()){
				return true;
			} else tkerr("expression expected after =");
		} else {
			iTk=start;
			return exprOr();
		}
	}
	return false;
}
// exprOr: exprAnd exprOrPrim
bool exprOr(){
	Token *start=iTk;
	if(exprAnd()){
		if(exprOrPrim()){
			return true;
		} else tkerr("syntax error in || expression");
	}
	iTk=start;
	return false;
}
// exprOrPrim: OR exprAnd exprOrPrim | epsilon
bool exprOrPrim(){
	if(consume(OR)){
		if(exprAnd()){
			if(exprOrPrim()){
				return true;
			} else tkerr("syntax error in || expression");
		} else tkerr("expression expected after ||");
	}
	return true;
}
// exprAnd: exprEq exprAndPrim
bool exprAnd(){
	Token *start=iTk;
	if(exprEq()){
		if(exprAndPrim()){
			return true;
		} else tkerr("syntax error in && expression");
	}
	iTk=start;
	return false;
}
// exprAndPrim: AND exprEq exprAndPrim | epsilon
bool exprAndPrim(){
	if(consume(AND)){
		if(exprEq()){
			if(exprAndPrim()){
				return true;
			} else tkerr("syntax error in && expression");
		} else tkerr("expression expected after &&");
	}
	return true;
}
// exprEq: exprRel exprEqPrim
bool exprEq(){
	Token *start=iTk;
	if(exprRel()){
		if(exprEqPrim()){
			return true;
		} else tkerr("syntax error in == or != expression");
	}
	iTk=start;
	return false;
}
// exprEqPrim: (EQUAL|NEQUAL) exprRel exprEqPrim | epsilon
bool exprEqPrim(){
	if(consume(EQUAL)){
		if(exprRel()){
			if(exprEqPrim()){
				return true;
			} else tkerr("syntax error in == expression");
		} else tkerr("expression expected after ==");
	}
	if(consume(NOTEQ)){
		if(exprRel()){
			if(exprEqPrim()){
				return true;
			} else tkerr("syntax error in != expression");
		} else tkerr("expression expected after !=");
	}
	return true;
}
// exprRel: exprAdd exprRelPrim
bool exprRel(){
	Token *start=iTk;
	if(exprAdd()){
		if(exprRelPrim()){
			return true;
		} else tkerr("syntax error in rel expression");
	}
	iTk=start;
	return false;
}
// exprRelPrim: (LESS|GREATER|LESSEQ|GREATEREQ) exprAdd exprRelPrim | epsilon
bool exprRelPrim(){
	if(consume(LESS)){
		if(exprAdd()){
			if(exprRelPrim()){
				return true;
			} else tkerr("syntax error in < expression");
		} else tkerr("expression expected after < operator");
	}
	if(consume(GREATER)){
		if(exprAdd()){
			if(exprRelPrim()){
				return true;
			} else tkerr("syntax error in > expression");
		} else tkerr("expression expected after > operator");
	}
	if(consume(LESSEQ)){
		if(exprAdd()){
			if(exprRelPrim()){
				return true;
			} else tkerr("syntax error in <= expression");
		} else tkerr("expression expected after <= operator");
	}
	if(consume(GREATEREQ)){
		if(exprAdd()){
			if(exprRelPrim()){
				return true;
			} else tkerr("syntax error in >= expression");
		} else tkerr("expression expected after >= operator");
	}
	return true;
}
// exprAdd: exprMul exprAddPrim
bool exprAdd(){
	Token *start=iTk;
	if(exprMul()){
		if(exprAddPrim()){
			return true;
		} else tkerr("syntax error in add expression");
	}
	iTk=start;
	return false;
}
// exprAddPrim: (ADD|SUB) exprMul exprAddPrim | epsilon
bool exprAddPrim(){
	if(consume(ADD)){
		if(exprMul()){
			if(exprAddPrim()){
				return true;
			} else tkerr("syntax error in add(+) expression");
		} else tkerr("expression expected after + ");
	}
	if(consume(SUB)){
		if(exprMul()){
			if(exprAddPrim()){
				return true;
			} else tkerr("syntax error in sub(-) expression");
		} else tkerr("expression expected after -");
	}
	return true;
}
// exprMul: exprCast exprMulPrim
bool exprMul(){		
	Token *start=iTk;
	if(exprCast()){
		if(exprMulPrim()){
			return true;
		} else tkerr("syntax error in mul expression");
	}
	iTk=start;
	return false;
}
// exprMulPrim: (MUL|DIV) exprCast exprMulPrim | epsilon
bool exprMulPrim(){
	if(consume(MUL)){
		if(exprCast()){
			if(exprMulPrim()){
				return true;
			} else tkerr("syntax error in mul(*) expression");
		} else tkerr("expression expected after * ");
	}
	if(consume(DIV)){
		if(exprCast()){
			if(exprMulPrim()){
				return true;
			} else tkerr("syntax error in div(/) expression");
		} else tkerr("expression expected after /");
	}
	return true;
}
//exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(){
	Token *start=iTk;
	if(consume(LPAR)){
		Type t;
		if(typeBase(&t)){
			if(arrayDecl(&t)){}
			if(consume(RPAR)){
				if(exprCast()){
					return true;
				} else tkerr("expression expected after cast");
			} else tkerr(") expected to close cast");
		} else tkerr("type expected in cast");
	}
	iTk=start;
	return exprUnary();
}
//exprUnary: (SUB|NOT) exprUnary | exprPostfix
bool exprUnary(){
	Token *start=iTk;
	if(consume(SUB)){
		if(exprUnary()){
			return true;
		} else tkerr("expression expected after - operator");
	}
	if(consume(NOT)){
		if(exprUnary()){
			return true;
		} else tkerr("expression expected after ! operator");
	}
	iTk=start;
	return exprPostfix();
}
// exprPostfix: exprPrimary exprPostfixPrim
bool exprPostfix() {
    Token *start = iTk;
    if (exprPrimary()) {
        if (exprPostfixPrim()) {
            return true;
        }else tkerr("syntax error in postfix expression");
    }
    iTk = start;
    return false;
}
// exprPostfixPrim: ( LBRACKET expr RBRACKET | DOT ID ) exprPostfixPrim | epsilon
bool exprPostfixPrim() {
	if (consume(LBRACKET)) {
		if (expr()) {
			if (consume(RBRACKET)) {
				if (exprPostfixPrim()) {
					return true;
				} else tkerr("syntax error in postfix expression");
			} else tkerr("] expected after array index");
		} else tkerr("expression expected in array index");
	} else if (consume(DOT)) {
		if (consume(ID)) {
			if (exprPostfixPrim()) {
				return true;
			} else tkerr("syntax error in postfix expression");
		} else tkerr("identifier expected after .");
	}
	return true;
}
// exprPrimary:  ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?| INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
bool exprPrimary(){
	Token *start=iTk;
	if(consume(ID)){
		if(consume(LPAR)){
			if(expr()){
				while(consume(COMMA)){
					if(!expr()) tkerr("expression expected after comma in argument list");
				}
			}
			if(consume(RPAR)){
				return true;
			} else tkerr(") expected to close argument list");
		} else {
			return true;
		}
	}
	else if(consume(INT)){return true;}
	else if(consume(DOUBLE)){return true;}
	else if(consume(CHAR)){return true;}
	else if(consume(STRING)){return true;}
	else if(consume(LPAR)){
		if(expr()){
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

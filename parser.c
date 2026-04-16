#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

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
bool typeBase();
bool arrayDecl();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound();

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
bool typeBase(){
	if(consume(TYPE_INT)){return true;}
	if(consume(TYPE_DOUBLE)){return true;}
	if(consume(TYPE_CHAR)){return true;}
	if(consume(STRUCT))
	{
		if(consume(ID)){return true;}
		else tkerr("identifier expected after struct");
	}
	return false;
}

bool structDef(){
	Token *start=iTk;
	if(consume(STRUCT)){
		if(consume(ID)){
			if(consume(LACC)){
				while(varDef()){}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
					return true;
				} else tkerr("; expected after struct definition");
				} else tkerr("} expected to close struct definition");
			} else tkerr("{ expected to open struct definition");
		} else tkerr("identifier expected after struct");
	}
	iTk=start;
	return false;
}

bool varDef(){
	Token *start=iTk;
	if(typeBase()){
		if(consume(ID)){
			if(arrayDecl()){}
			if(consume(SEMICOLON)){
				return true;
			} else tkerr("; expected after variable definition");
		} else tkerr("identifier expected after type");
	}
	iTk=start;
	return false;
}

bool arrayDecl(){
	Token *start=iTk;
	if(consume(LBRACKET)){
		if(consume(INT)){}
		if(consume(RBRACKET)){
				return true;
			} else tkerr("] expected after array size");
		} 
	iTk=start;
	return false;
}

bool fnDef(){
	Token *start=iTk;
	if(typeBase() || consume(VOID)){
		if(consume(ID)){
			if(consume(LPAR)){
				if(fnParam()){}
				while(consume(COMMA)){
					if(!fnParam())tkerr("parameter expected after comma");
				}
				if(consume(RPAR)){
					if(stmCompound()){
						return true;
					} else tkerr("function body expected after function declaration");
				} else tkerr(") expected to close parameter list");
			} else tkerr("( expected to open parameter list");
		} else tkerr("identifier expected after type");
	}
	iTk=start;
	return false;
}

bool fnParam(){
	Token *start=iTk;
	if(typeBase()){
		if(consume(ID)){
			if(arrayDecl()){}
			return true;
		} else tkerr("identifier expected after type in parameter");
	}
	iTk=start;
	return false;
}

bool stm(){
	Token *start=iTk;
	if(stmCompound()) return true;

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
			} else tkerr("expression expected in if condition");
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
			} else tkerr("expression expected in while condition");
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

bool stmCompound(){
	Token *start=iTk;
	if(consume(LACC)){
		while(varDef() || stm()){}
		if(consume(RACC)){
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
	if(consume(EQUAL) || consume(NOTEQ)){
		if(exprRel()){
			if(exprEqPrim()){
				return true;
			} else tkerr("syntax error in == or != expression");
		} else tkerr("expression expected after == or !=");
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
	if(consume(LESS) || consume(GREATER) || consume(LESSEQ) || consume(GREATEREQ)){
		if(exprAdd()){
			if(exprRelPrim()){
				return true;
			} else tkerr("syntax error in rel expression");
		} else tkerr("expression expected after relational operator");
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
	if(consume(ADD) || consume(SUB)){
		if(exprMul()){
			if(exprAddPrim()){
				return true;
			} else tkerr("syntax error in add expression");
		} else tkerr("expression expected after + or -");
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
	if(consume(MUL) || consume(DIV)){
		if(exprCast()){
			if(exprMulPrim()){
				return true;
			} else tkerr("syntax error in mul expression");
		} else tkerr("expression expected after * or /");
	}
	return true;
}
//exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
bool exprCast(){
	Token *start=iTk;
	if(consume(LPAR)){
		if(typeBase()){
			if(arrayDecl()){}
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
	if(consume(SUB) || consume(NOT)){
		if(exprUnary()){
			return true;
		} else tkerr("expression expected after unary operator");
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
	iTk=start;
	return false;
}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
}

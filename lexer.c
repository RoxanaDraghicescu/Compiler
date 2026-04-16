#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin,const char *end){
	size_t len=end-begin;
	char *str=safeAlloc(len+1);
	strncpy(str,begin,len);
	str[len]='\0';
	return str;
	}

Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	for(;;){
		switch(*pch){
			case ' ':case '\t':pch++;break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case '\0':addTk(END);return tokens;
			case ',':addTk(COMMA);pch++;break;
			case ';':addTk(SEMICOLON);pch++;break;
			case '(': addTk(LPAR);pch++;break;
			case ')':addTk(RPAR);pch++;break;
			case '[':addTk(LBRACKET);pch++;break;
			case ']':addTk(RBRACKET);pch++;break;
			case '{':addTk(LACC);pch++;break;
			case '}':addTk(RACC);pch++;break;
			case '=':
				if(pch[1]=='='){
					addTk(EQUAL);
					pch+=2;
					}else{
					addTk(ASSIGN);
					pch++;
					}
				break;
			case '+': addTk(ADD); pch++; break;
			case '-': addTk(SUB); pch++; break;
			case '*': addTk(MUL); pch++; break;
			case '.' :addTk(DOT); pch++; break;
			case '/':
					if(pch[1]=='/'){
						pch+=2;
						while (*pch != '\n' && *pch != '\r' && *pch != '\0') pch++;
					}
					else{
						addTk(DIV);
						pch++;
					}
					break;
			case '!':
					if(pch[1]=='='){addTk(NOTEQ); pch+=2;}
					else{addTk(NOT);pch++;}
					break;
			case '<':
					if(pch[1]=='='){addTk(LESSEQ); pch+=2;}
					else{addTk(LESS); pch++;}
					break;
			case '>':
					if(pch[1]=='='){addTk(GREATEREQ); pch+=2;}
					else{addTk(GREATER); pch++;}
					break;
			case '&': 
					if(pch[1]=='&'){addTk(AND); pch+=2;}
					else{err("invalid char: %c(%d)", *pch,*pch);}
					break;
			case '|': 
					if(pch[1]=='|'){addTk(OR); pch+=2;}
					else{err("invalid char: %c(%d)", *pch,*pch);}
					break;	
			//CHAR
			case '\'': 
			{
                pch++;
                char c;
                if (*pch== '\\'){
                    pch++;
                    switch (*pch){
                        case 'a': c='\a'; break; case 'b': c='\b'; break;
                        case 'f': c='\f'; break; case 'n': c='\n'; break;
                        case 'r': c='\r'; break; case 't': c='\t'; break;
                        case 'v': c='\v'; break; case '\\': c='\\'; break;
                        case '\'': c='\''; break; case '\"': c='\"'; break;
                        case '0': c='\0'; break;
                        default: err("invalid escape sequence in char");
                    }
                    pch++;
                } 
				else{c = *pch++;}
                if(*pch!='\'') err("missing ' at end of char constant");
                pch++;
                tk=addTk(CHAR);
                tk->c=c;
                break;
            }

            //STRING
            case '"': 
			{
                pch++;
                int len = 0;
                const char *tmp=pch;
                while (*tmp!='"' && *tmp!='\0') {
                    if(*tmp=='\\') tmp+=2;
                    else tmp++;
                    len++;
                }
                if (*tmp=='\0') err("missing \" at end of string constant");
                
                char *str=safeAlloc(len+1);
                int i=0;
                while (*pch!='"') {
                    if (*pch=='\\') 
					{
                        pch++;
                        switch (*pch) 
						{
                            case 'a': str[i++]='\a'; break; case 'b': str[i++]='\b'; break;
                            case 'f': str[i++]='\f'; break; case 'n': str[i++]='\n'; break;
                            case 'r': str[i++]='\r'; break; case 't': str[i++]='\t'; break;
                            case 'v': str[i++]='\v'; break; case '\\': str[i++]='\\'; break;
                            case '\'': str[i++]='\''; break; case '\"': str[i++]='\"'; break;
                            case '0': str[i++]='\0'; break;
                            default: err("invalid escape sequence in string");
                        }
                    } else {str[i++] = *pch;}
                    pch++;
                }
                str[i] ='\0';
                pch++; // Consuma ghilimelele de final
                tk=addTk(STRING);
                tk->text=str;
                break;
            }
			default:
			//ID si Cuv cheie
				if(isalpha(*pch)||*pch=='_')
				{
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
					char *text=extract(start,pch);
					if(strcmp(text,"char")==0)addTk(TYPE_CHAR);
					else if (strcmp(text,"double")==0)addTk(TYPE_DOUBLE);
					else if (strcmp(text,"else")==0)addTk(ELSE);
					else if (strcmp(text,"if")==0)addTk(IF);
					else if (strcmp(text,"int")==0)addTk(TYPE_INT);
					else if (strcmp(text,"return")==0)addTk(RETURN);
					else if (strcmp(text,"struct")==0)addTk(STRUCT);
					else if (strcmp(text,"void")==0)addTk(VOID);
					else if (strcmp(text,"while")==0)addTk(WHILE);
					else{
						tk=addTk(ID);
						tk->text=text;
						text=NULL;
						}
					if(text) free(text);
				}
			//Const int si double
				else if(isdigit(*pch))
				{
					start=pch;
					int isDouble=0;
					while(isdigit(*pch))pch++;
					if(*pch=='.'){
						isDouble=1;
						pch++;
						if(!isdigit(*pch)) err("missing digits after decimal point in number");
						while(isdigit(*pch))pch++;
					}
					if(*pch =='e' || *pch=='E'){
						isDouble=1;
						pch++;
						if(*pch=='+' || *pch=='-') pch++;
						if(!isdigit(*pch)) err("missing digits after exponent in number");
						while(isdigit(*pch))pch++;
					}
					char *text=extract(start,pch);
					if(isDouble){tk=addTk(DOUBLE); tk->d=strtod(text,NULL);}
					else{tk=addTk(INT); tk->i=strtol(text,NULL,10);}
					free(text);
				}
				else err("invalid char: %c (%d)",*pch,*pch);
			}
		}
	}
const char *tkNames[] = {
    "ID", "TYPE_CHAR", "TYPE_DOUBLE", "ELSE", "IF", "TYPE_INT", "RETURN", "STRUCT", "VOID", "WHILE",
    "INT", "DOUBLE", "CHAR", "STRING",
    "COMMA", "END", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC",
    "ASSIGN", "EQUAL", "ADD", "SUB", "MUL", "DIV", "DOT", "AND", "OR", "NOT", "NOTEQ", "LESS", "LESSEQ", "GREATER", "GREATEREQ"
};
void showTokens(const Token *tokens){
	for(const Token *tk=tokens;tk;tk=tk->next)
	{
		printf("%d\t%s", tk->line, tkNames[tk->code]);
		if(tk->code==INT){printf(":%d",tk->i);}
		else if(tk->code==DOUBLE){printf(":%.2f",tk->d);}
		else if(tk->code==CHAR){printf(":%c",tk->c);}
		else if(tk->code ==STRING || tk->code==ID){printf(":%s",tk->text);}

		printf("\n");
	}
}

letra [a-zA-Z]
letra_ {letra}|_
digito [0-9]
letraDigito_ {letra_}|{digito}
letras {letra}+
suffixo "."{letras}
suffixos {suffixo}+
emailID {letra_}{letraDigito_}*
email {emailID}"@"{letras}{suffixos}

digitos2 {digito}{2}
digitos3 {digitos2}{digito}
digitos11 {digito}{11}

cpf ({digitos3}"."{digitos3}"."{digitos3}"-"{digitos2})|{digitos11}

%%
{email} {
    printf("email <%s> encontrado!\n", yytext);
}
{cpf} {
    printf("cpf <%s> encontrado!\n", yytext);
}
. printf("entrada desconhecida!\n");
%%

int yywrap(){
    return 0;
}

int main() {
    yylex();
}
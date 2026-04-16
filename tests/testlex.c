// program de testare a analizorului lexical, v1.1

int main()
{
	int i;
	i=0;
	while(i<10){
		if(i/2==1)puti(i);
		i=i+1;
		}
	if(4.9==49e-4&&0.49E1==2.45*2.0)puts("yes");
	putc('\'');
	puts("pentru \r\n si tab \t testam");	// pentru \n
	double c=4.5;
	return 0;
}

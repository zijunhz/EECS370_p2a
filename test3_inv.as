	lw	0	1	five	load reg1 with 5 (symbolic address)
	lw	1	2	3	load reg2 with -1 (numeric address)
start	add	1	2	1	decrement reg1
	beq	0	1	32767	goto end of program when reg1==0
	beq	0	0	Start	go back to the beginning of the loop
	noop
done	halt				end of program
self	lw	0	3	self	this is to test
five	.fill	5
Five	.fill	999999
neg1	.fill	-1
stAddr	.fill	start			will contain the address of start (2)

	lw	0	1	five	load reg1 with 5 (symbolic address)
	sw	1	2	3	load reg2 with -1 (numeric address)
	lw	0	1	five	load reg1 with 5 (symbolic address)
	sw	1	2	3	load reg2 with -1 (numeric address)
startt	nor	4	4	2
start	add	1	2	1	decrement reg1
	add	1	2	1	decrement reg1
	beq	0	1	2	goto end of program when reg1==0
	beq	0	0	start	go back to the beginning of the loop
	beq	2	7	done	go back to the beginning of the loop
	lw	2	4	start
	sw	6	4	32764
	noop
	beq	0	1	2	goto end of program when reg1==0
	beq	0	0	start	go back to the beginning of the loop
	beq	2	7	done	go back to the beginning of the loop
	lw	2	4	start
	sw	6	4	32764
	noop
	beq	0	1	2	goto end of program when reg1==0
	beq	0	0	start	go back to the beginning of the loop
	beq	2	7	done	go back to the beginning of the loop
	lw	2	4	start
	sw	6	4	32764
	noop
done	halt				end of program
self	lw	0	3	self	this is to test
five	.fill	5
neg1	.fill	-1
stAddr	.fill	start			will contain the address of start (2)

	.text
	.file	"adder.ll"
	.globl	add
	.align	16, 0x90
	.type	add,@function
add:                                    # @add
	.cfi_startproc
# BB#0:
                                        # kill: ESI<def> ESI<kill> RSI<def>
                                        # kill: EDI<def> EDI<kill> RDI<def>
	leal	(%rdi,%rsi), %eax
	retq
.Ltmp0:
	.size	add, .Ltmp0-add
	.cfi_endproc

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	xorl	%edi, %edi
	movl	$97, %esi
	callq	add
	movl	%eax, %edi
	callq	putchar
	popq	%rax
	retq
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc


	.section	".note.GNU-stack","",@progbits

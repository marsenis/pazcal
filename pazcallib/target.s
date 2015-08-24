	.file "examples/read.pzc"
	.section .rodata
.STR0:
	.string	"Makis"
	.text
L1:
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$12,%rsp
L2:
	movq	$.STR0, %rdi
L3:
L4:
	call	strlen
	movl	%eax, -4(%rbp)
L5:
	movl	-4(%rbp), %edi # tmp1
L6:
	movl	$1, %esi
L7:
	call	WRITE_INT
L8:
	movb	$0xa, %dil
L9:
	movl	$1, %esi
L10:
	call	WRITE_CHAR
L11:
.main:
	movq	%rbp, %rsp
	popq	%rbp
	ret

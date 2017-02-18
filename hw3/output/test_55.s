	.data
	.globl	_gbl
_gbl:
	.quad	12
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rdi
	pushq	%rsi
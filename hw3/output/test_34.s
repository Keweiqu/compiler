	.data
	.globl	_hd
_hd:
	.quad	1
	.quad	_md
	.data
	.globl	_md
_md:
	.quad	2
	.quad	_tl
	.data
	.globl	_tl
_tl:
	.quad	3
	.quad	0
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rdi
	pushq	%rsi
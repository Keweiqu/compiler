	.data
	.globl	_gstr
_gstr:
	.asciz	"hello, world!"
	.data
	.globl	_gint
_gint:
	.quad	42
	.data
	.globl	_v1
_v1:
	.quad	0
	.quad	_gint
	.data
	.globl	_v2
_v2:
	.quad	1
	.quad	0
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rdi
	pushq	%rsi
	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rdi
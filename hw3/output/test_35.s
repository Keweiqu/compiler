	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	.text
	.globl	bar
bar:
	pushq	%rbp
	movq	%rsp, %rbp
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rdi
	pushq	%rsi
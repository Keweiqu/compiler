	.data
	.globl	gbl
gbl:
	.quad	1
	.quad	2
	.quad	3
	.quad	4
	.quad	5
	.quad	6
	.quad	7
	.text
	.globl	main
main:
	movq	%rsp, %rbp
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	pushq	%rdi
	pushq	%rsi
	movq	%rbp, %rsp
	addq	$-80, %rsp
	movq	$gbl, %r14
	movq	$56, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	addq	$8, %r14
	movq	$8, %r10
	movq	$2, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -64(%rbp)
	movq	-64(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -72(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-72(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
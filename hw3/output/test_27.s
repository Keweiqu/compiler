	.data
	.globl	tmp
tmp:
	.quad	1
	.quad	2
	.quad	3
	.quad	4
	.quad	5
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
	addq	$-88, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	$tmp, %r14
	movq	$40, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	$8, %r10
	movq	$3, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -72(%rbp)
	movq	-72(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -80(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-80(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
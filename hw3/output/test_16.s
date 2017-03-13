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
	addq	$-96, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	$17, %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	%rsp, -72(%rbp)
	addq	$8, %rsp
	movq	-64(%rbp), %r11
	movq	-72(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	-72(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -80(%rbp)
	movq	-80(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -88(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-88(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
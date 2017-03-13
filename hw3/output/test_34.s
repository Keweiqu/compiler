	.data
	.globl	hd
hd:
	.quad	1
	.quad	md
	.data
	.globl	md
md:
	.quad	2
	.quad	tl
	.data
	.globl	tl
tl:
	.quad	3
	.quad	0
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
	addq	$-128, %rsp
	movq	$hd, %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -64(%rbp)
	movq	$hd, %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	addq	$8, %r14
	movq	%r14, -72(%rbp)
	movq	-72(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -80(%rbp)
	movq	-80(%rbp), %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -88(%rbp)
	movq	-80(%rbp), %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	addq	$8, %r14
	movq	%r14, -96(%rbp)
	movq	-96(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -104(%rbp)
	movq	-104(%rbp), %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -112(%rbp)
	movq	-112(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -120(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-120(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
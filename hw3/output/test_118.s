	.data
	.globl	gstr
gstr:
	.asciz	"hello, world!"
	.data
	.globl	gint
gint:
	.quad	42
	.data
	.globl	v1
v1:
	.quad	0
	.quad	gint
	.data
	.globl	v2
v2:
	.quad	1
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
	addq	$-96, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	$v2, %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -72(%rbp)
	movq	$5, %r11
	movq	-72(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	$v2, %r11
	movq	%r11, -80(%rbp)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-80(%rbp), %rdi
	callq	foo
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	-72(%rbp), %rcx
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
	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	pushq	%rdi
	movq	%rbp, %rsp
	addq	$-72, %rsp
	movq	-48(%rbp), %r14
	movq	$16, %r10
	movq	$0, %r11
	imulq	%r10, %r11
	addq	%r11, %r14
	movq	%r14, -56(%rbp)
	movq	$6, %r11
	movq	-56(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	-56(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -64(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
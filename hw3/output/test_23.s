	.text
	.globl	bar
bar:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	pushq	%r8
	pushq	%r9
	pushq	16(%rbp)
	pushq	24(%rbp)
	movq	%rbp, %rsp
	addq	$-168, %rsp
	movq	-56(%rbp), %rcx
	movq	-48(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -112(%rbp)
	movq	-64(%rbp), %rcx
	movq	-112(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -120(%rbp)
	movq	-72(%rbp), %rcx
	movq	-120(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -128(%rbp)
	movq	-80(%rbp), %rcx
	movq	-128(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -136(%rbp)
	movq	-88(%rbp), %rcx
	movq	-136(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -144(%rbp)
	movq	-96(%rbp), %rcx
	movq	-144(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -152(%rbp)
	movq	-104(%rbp), %rcx
	movq	-152(%rbp), %r11
	addq	%rcx, %r11
	movq	%r11, -160(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-160(%rbp), %rax
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
	addq	$-64, %rsp
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	-48(%rbp), %rdi
	movq	-48(%rbp), %rsi
	movq	-48(%rbp), %rdx
	movq	-48(%rbp), %rcx
	movq	-48(%rbp), %r8
	movq	-48(%rbp), %r9
	pushq	-48(%rbp)
	pushq	-48(%rbp)
	callq	bar
	addq	$80, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -56(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-56(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
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
	addq	$-72, %rsp
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	$3, %rdi
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
	movq	%rax, -64(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-64(%rbp), %rax
	movq	%rbp, %rsp
	addq	$8, %rsp
	movq	(%rbp), %rbp
	retq	
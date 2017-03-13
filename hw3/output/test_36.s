	.text
	.globl	factorial
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	pushq	%rdi
	movq	%rbp, %rsp
	addq	$-136, %rsp
	movq	%rsp, -56(%rbp)
	addq	$8, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	-48(%rbp), %r11
	movq	-56(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	$1, %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	jmp	start
	.text
start:
	movq	-56(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -72(%rbp)
	movq	$0, -80(%rbp)
	movq	-72(%rbp), %r11
	cmpq	$0, %r11
	setg	-80(%rbp)
	cmpq	$0, -80(%rbp)
	je	end
	jne	then
	.text
then:
	movq	-64(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -88(%rbp)
	movq	-56(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -96(%rbp)
	movq	-96(%rbp), %rcx
	movq	-88(%rbp), %r11
	imulq	%rcx, %r11
	movq	%r11, -104(%rbp)
	movq	-104(%rbp), %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	movq	-56(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -112(%rbp)
	movq	$1, %rcx
	movq	-112(%rbp), %r11
	subq	%rcx, %r11
	movq	%r11, -120(%rbp)
	movq	-120(%rbp), %r11
	movq	-56(%rbp), %rcx
	movq	%r11, (%rcx)
	jmp	start
	.text
end:
	movq	-64(%rbp), %rcx
	movq	(%rcx), %r11
	movq	%r11, -128(%rbp)
	movq	-40(%rbp), %r15
	movq	-32(%rbp), %r14
	movq	-24(%rbp), %r13
	movq	-16(%rbp), %r12
	movq	-8(%rbp), %rbx
	movq	-128(%rbp), %rax
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
	addq	$-80, %rsp
	movq	%rsp, -64(%rbp)
	addq	$8, %rsp
	movq	$0, %r11
	movq	-64(%rbp), %rcx
	movq	%r11, (%rcx)
	pushq	%r11
	pushq	%r10
	pushq	%r9
	pushq	%r8
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%rcx
	movq	$5, %rdi
	callq	factorial
	addq	$24, %rsp
	movq	-8(%rsp), %r11
	movq	-16(%rsp), %r10
	movq	-24(%rsp), %r9
	movq	-32(%rsp), %r8
	movq	-40(%rsp), %rdi
	movq	-48(%rsp), %rsi
	movq	-56(%rsp), %rdx
	movq	-64(%rsp), %rcx
	movq	%rax, -72(%rbp)
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
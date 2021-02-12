        .global puzzle6
        .text

puzzle6:
        movq        (%rdi), %rdx
        leaq        8(%rdi), %rcx
        xorl        %eax, %eax
        vxorps      %xmm1, %xmm1, %xmm1
        vmovss      .LC1(%rip), %xmm2
.L2:    cmpq        %rdx, %rax
        jge         .L5
        vfmadd231ss (%rcx,%rax,4), %xmm2, %xmm1
        incq        %rax
        vmulss      %xmm0, %xmm2, %xmm2
        jmp         .L2
.L5:    vmovaps     %xmm1, %xmm0
        ret

.LC1:   .long       0x3f800000

# vim: set expandtab:

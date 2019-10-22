hnit:
        js main

fib_init:
        li s0, 0
        swsp s0, 2(sp)
        li s1, 1
        swsp s1, 2(sp)
        li s2, 50
        jr ra

fib_main:
        add2 s0 ,s1
        blt s2, s1, fib_return
        swsp s0, 2(sp)
        add2 s1, s0
        bleu s2, s1, fib_return
        swsp s1, 2(sp)
        js fib_main
fib_return:
        jr ra
main:
        jsal fib_init
        jsal fib_main
        swsp s0, 0(sp)
        lwsp x8, 0(sp)
        nop
        js 0

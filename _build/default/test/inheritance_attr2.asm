.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
init_end:
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, -16
	li $t0, 1
	la $t1, _0
	sw $t0, 0($t1)
	li $t0, 2
	la $t1, _1
	sw $t0, 0($t1)
	li $t0, 16
	move $a0, $t0
	li $v0, 9
	syscall
	move $t0, $v0
	la $t1, _2
	sw $t0, 0($t1)
	la $t0, _2
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, point_descr
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	la $t6, _1
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	la $t6, _0
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	la $t6, _2
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	jal point_constructor
	addi $sp, $sp, 12
	la $t0, _2
	lw $t0, 0($t0)
	sw $t0, -8($fp)
	li $t0, 5
	la $t1, _3
	sw $t0, 0($t1)
	li $t0, -5
	la $t1, _4
	sw $t0, 0($t1)
	li $t0, 16
	move $a0, $t0
	li $v0, 9
	syscall
	move $t0, $v0
	la $t1, _5
	sw $t0, 0($t1)
	la $t0, _5
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, point_descr
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	la $t6, _4
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	la $t6, _3
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	la $t6, _5
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	jal point_constructor
	addi $sp, $sp, 12
	la $t0, _5
	lw $t0, 0($t0)
	sw $t0, -12($fp)
	lw $t0, -8($fp)
	li $t0, 48
	la $t1, _6
	sw $t0, 0($t1)
	la $t6, _6
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	lw $t6, -8($fp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	li $t0, 16
	lw $t1, -8($fp)
	lw $t1, 0($t1)
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	jalr $t0
	addi $sp, $sp, 8
	la $t1, _7
	sw $t0, 0($t1)
	la $t0, _7
	lw $t0, 0($t0)
	sw $t0, -16($fp)
	li $t0, 5
	la $t1, _8
	sw $t0, 0($t1)
	li $t0, 16
	move $a0, $t0
	li $v0, 9
	syscall
	move $t0, $v0
	la $t1, _9
	sw $t0, 0($t1)
	la $t0, _9
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, carre_descr
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	la $t6, _8
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	la $t6, _9
	lw $t6, 0($t6)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	jal carre_constructor
	addi $sp, $sp, 8
	la $t0, _9
	lw $t0, 0($t0)
	sw $t0, -20($fp)
	lw $t0, -12($fp)
	lw $t6, -12($fp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	li $t0, 4
	lw $t1, -12($fp)
	lw $t1, 0($t1)
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	jalr $t0
	addi $sp, $sp, 4
	la $t1, _10
	sw $t0, 0($t1)
	la $t0, _10
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 11
	syscall
	lw $t0, -8($fp)
	lw $t6, -8($fp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	li $t0, 4
	lw $t1, -8($fp)
	lw $t1, 0($t1)
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	jalr $t0
	addi $sp, $sp, 4
	la $t1, _11
	sw $t0, 0($t1)
	la $t0, _11
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 11
	syscall
	lw $t0, -20($fp)
	lw $t6, -20($fp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	li $t0, 4
	lw $t1, -20($fp)
	lw $t1, 0($t1)
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	jalr $t0
	addi $sp, $sp, 4
	la $t1, _12
	sw $t0, 0($t1)
	la $t0, _12
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 11
	syscall
	lw $t0, -16($fp)
	lw $t0, -16($fp)
	move $a0, $t0
	li $v0, 11
	syscall
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
circle_get_z:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	li $t0, 97
	li $t1, 1
	li $t2, 4
	mul $t1, $t2, $t1
	lw $t2, 4($fp)
	add $t1, $t2, $t1
	lw $t1, 0($t1)
	add $t0, $t1, $t0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
circle_test:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	li $t0, 1
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
point_constructor:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	li $t0, 2
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 8($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	li $t0, 3
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 12($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	li $t0, 1
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 12($fp)
	lw $t1, 8($fp)
	add $t0, $t1, $t0
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
point_sum:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	li $t0, 3
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	li $t1, 2
	li $t2, 4
	mul $t1, $t2, $t1
	lw $t2, 4($fp)
	add $t1, $t2, $t1
	lw $t1, 0($t1)
	lw $t2, 8($fp)
	add $t1, $t2, $t1
	add $t0, $t1, $t0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
carre_constructor:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	li $t0, 2
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 8($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	li $t0, 3
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 8($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	li $t0, 1
	li $t1, 4
	mul $t0, $t1, $t0
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 8($fp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	sw $t0, 0($t1)
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
#built-in atoi
atoi:
	li $v0, 0
atoi_loop:
	lbu $t0, 0($a0)
	beqz $t0, atoi_end
	addi $t0, $t0, -48
	bltz $t0, atoi_error
	bge $t0, 10, atoi_error
	mul $v0, $v0, 10
	add $v0, $v0, $t0
	addi $a0, $a0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	jr $ra
.data
_6:
	.word 0
_7:
	.word 0
_11:
	.word 0
_3:
	.word 0
_4:
	.word 0
_1:
	.word 0
_0:
	.word 0
_9:
	.word 0
_10:
	.word 0
_5:
	.word 0
_8:
	.word 0
_2:
	.word 0
_12:
	.word 0
circle_descr:
	.word 0
	.word circle_get_z
	.word circle_test
point_descr:
	.word circle_descr
	.word circle_get_z
	.word circle_test
	.word point_constructor
	.word point_sum
carre_descr:
	.word point_descr
	.word circle_get_z
	.word circle_test
	.word point_sum
	.word carre_constructor

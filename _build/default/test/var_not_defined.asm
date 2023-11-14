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
	addi $sp, $sp, -4
	subi $sp, $sp, 4
	sw $s0, 0($sp)
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
#here 0
	la $t0, point_descr
#here 1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
#here 2
	sw $t0, 0($t1)
#Start tr_params Call
	la $t0, _1
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, _0
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	la $t0, _2
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#Start save Call
#End save Call
	jal point_constructor
#Start restore Call
#End restore Call
	addi $sp, $sp, 12
	la $t0, _2
	lw $t0, 0($t0)
	move $s0, $t0
	li $t0, 97
	la $t1, _3
	sw $t0, 0($t1)
	move $t0, $s0
	move $t0, $s0
	li $t1, 4
	li $t2, 1
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	la $t1, _4
	sw $t0, 0($t1)
	la $t0, _3
	lw $t0, 0($t0)
	la $t1, _4
	lw $t1, 0($t1)
	add $t0, $t0, $t1
	la $t1, _5
	sw $t0, 0($t1)
	la $t0, _5
	lw $t0, 0($t0)
	move $a0, $t0
	li $v0, 11
	syscall
	lw $s0, 0($sp)
	addi $sp, $sp, 4
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	li $t0, 0
	jr $ra
circle_constructor:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	lw $t0, 4($fp)
	li $t1, 4
	li $t2, 1
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#here 0
	lw $t0, 8($fp)
#here 1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
#here 2
	sw $t0, 0($t1)
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	li $t0, 0
	jr $ra
point_constructor:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
#Start tr_params DCall
	lw $t0, 8($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t0, 4($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#Start save DCall
#End save DCall
	lw $t0, 4($fp)
	lw $t0, 0($t0)
	lw $t0, 0($t0)
	li $t1, 4
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	jalr $t0
#Start restore DCall
#End restore DCall
	addi $sp, $sp, 8
	lw $t0, 4($fp)
	li $t1, 4
	li $t2, 3
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#here 0
	lw $t0, 12($fp)
#here 1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
#here 2
	sw $t0, 0($t1)
	lw $t0, 4($fp)
	li $t1, 4
	li $t2, 2
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#here 0
	lw $t0, 8($fp)
#here 1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
#here 2
	sw $t0, 0($t1)
	lw $t0, 4($fp)
	li $t1, 4
	li $t2, 1
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#here 0
	lw $t0, 4($fp)
	li $t1, 4
	li $t2, 1
	mul $t1, $t1, $t2
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	li $t1, 4
	add $t0, $t0, $t1
#here 1
	lw $t1, 0($sp)
	addi $sp, $sp, 4
#here 2
	sw $t0, 0($t1)
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	li $t0, 0
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
_3:
	.word 0
_4:
	.word 0
_1:
	.word 0
_0:
	.word 0
_5:
	.word 0
_2:
	.word 0
circle_descr:
	.word 0
	.word circle_constructor
point_descr:
	.word circle_descr
	.word point_constructor

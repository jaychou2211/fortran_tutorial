program HW07
    ! 宣告一個 13row，8colume 的二維陣列
    INTEGER,DIMENSION(-6:6, 0:7) :: values 
    ! 正整數個數/負整數個數/0的個數/目前位於第幾個數
    INTEGER :: p_count = 0,    &
               n_count = 0,    &
               zero_count = 0, & 
               total_count = 1
    ! 用以創造整數(+、-、0)的兩個實數亂數
    REAL :: ranNum1,ranNum2

    ! 呼叫亂數種
    call random_seed()

    DO I = -6,6
        DO J = 0,7
            call random_number(ranNum1) ! 亂數實數1號
            call random_number(ranNum2) ! 亂數實數2號
            values(I,J) = NINT((ranNum1-ranNum2)*10) ! 相減*10四捨五入到整數位 ➜ 隨機的整數
            ! print'("No.", i3, " is:", i3)',total_count ,values(I,J)
            total_count = total_count +1 
        END DO
        WRITE(*,*) values(I,:)
    END DO


    DO I = -6,6     ! 第1層迴圈:row
        DO J = 0,7  ! 第2層迴圈:column
            ! 若站在內層迴圈看世界，會看見隨著J從0變到7，I都不變 ➜ 「一橫排做完後在做下一橫排」
            IF (values(I,J)>0) THEN
                p_count = p_count + 1
            ELSEIF (values(I,J)<0) THEN
                n_count = n_count + 1
            ELSE
                zero_count = zero_count + 1
            END IF
        END DO
    END DO


    PRINT '("positive integer:", i3)',p_count
    PRINT '("negtive integer:", i3)',n_count
    PRINT '("zero:", i3)',zero_count

STOP
end program HW07
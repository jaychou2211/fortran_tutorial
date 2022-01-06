program HW08_V1
    REAL,DIMENSION(4,7) :: a
    REAL :: sum = 0, &
            ave = 0 
    ! 一般我是不會在這裡先設置，但現在會設是因為第19行會用到J        
    INTEGER :: I,J 
    
    OPEN (15, FILE='hw08_1.txt', FORM='FORMATTED', STATUS='OLD')

    DO I = 1,4
        READ(15,*) (a(I,J), J=1,7)
        WRITE(*,*) a(I,:)
    END DO

    DO I = 1,4
        DO J = 1,7
            sum = sum + a(I,J)
        END DO
        ave = sum / J
        PRINT'("the sum of no.", i1, " raw : ", F12.7)', I, sum
        sum = 0
        PRINT'("the average of no.", i1, " raw : ", F12.7)', I, ave
        ave = 0
    END DO

    DO J = 1,7 
        DO I = 1,4 
            sum = sum + a(I,J)
        END DO
        ave = sum / I
        PRINT'("the sum of no.", i1, " column : ", F12.7)', J, sum
        sum = 0
        PRINT'("the average of no.", i1, " column : ", F12.7)', J, ave
        ave = 0
    END DO

    CLOSE(15)
    
end program HW08_V1
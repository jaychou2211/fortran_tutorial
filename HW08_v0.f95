program HW08_V0
    INTEGER,DIMENSION(1:6,1:5) :: a

    OPEN (10, FILE='test.txt', FORM='FORMATTED', STATUS='OLD')

        DO I = 1, 6
            READ(10,*) (A(I,J), J=1,5)
        END DO

    CLOSE(10)

    DO I = 1, 6
        WRITE(*,*) a(I,:)
    END DO
    
end program HW08_V0
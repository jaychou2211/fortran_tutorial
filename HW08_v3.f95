program HW08_V3
    ! 用來存入文件資料的二維動態陣列
    REAL,ALLOCATABLE,DIMENSION(:,:) :: a
    ! 即將被讀取檔案名稱
    CHARACTER(LEN =20) :: filename 
    ! 設計來用以推敲「總數、列數、行數」的字串
    CHARACTER(LEN = 1000) :: content     
    ! 變數1/變數2/數字總數/列/行
    INTEGER :: I = 1,      &
               J = 1,      &
               total = 0,  &
               row = 0,    &
               col = 0 
    ! 垂直方向的總和&平均/水平方向的總和&平均
    REAL,ALLOCATABLE,DIMENSION(:,:) :: row_sa,col_sa


    ! 輸入文件名稱 
    WRITE(*,9)
    9 FORMAT("Pleasa enter the name of your file :")
    READ(*,*) filename 
    

    ! 計算有幾個column
    OPEN (15, FILE= filename, FORM='FORMATTED', STATUS='OLD') 

        !由於沒有特別設定，默認會讀取資料進content，直到「換行」的前一刻
        READ(15,210) content 
        210 FORMAT(A300)
        
        ! 算出第一行究竟有多少個字元
        DO WHILE(content(I:(I+2)) /= "   ") ! 「21.」、「1.3」、「.3 」...... 「6  」
            ! WRITE(*,*) content(I:(I+2))
            col = col+1 
            I = I+1 ! 接著要讀後3個字元
        END DO
        ! 
        col = (col/6)+1  
        ! 除以6是因為21.3  22.6  ...，經過6個位置就會遇到下一個數字
        !          #123456123456
        ! 而 + 1 的原因在於最後21.6的部分不會走完6個位置

    CLOSE(15)

    ! 算出 資料總筆數、row
    ! 把文件裡的內容，透過每1000個字就切成一個區塊
    OPEN (15, FILE= filename, FORM='FORMATTED', STATUS='OLD',  ACCESS='direct', RECL=1000)
        
        ! (我record length是故意取1000的，這樣文件全部只會有一塊) ➜ 只要讀第一塊，就能直接讀到底
        READ(15,200, REC=1) content
        200 FORMAT(A300) 
        
        !每個實數都有小數點 ➜ 小數點總數 =  資料總筆數
        DO I = 1,300
            IF (content(I:I) == ".") THEN
                total = total + 1 
                ! print*,total
            END IF
        END DO  
        ! 得到row數
        row = total/col

        ! PRINT*, total,col,row

    CLOSE(15)

    !有了 row值 和 column值 ，現在我有辦法定義動態陣列了
    ALLOCATE(a(row,col))
    ALLOCATE(row_sa(row,2))
    ALLOCATE(col_sa(2,col))

    ! 讀取資料進入陣列a裡
    OPEN (15, FILE= filename, FORM='FORMATTED', STATUS='OLD')
        
        PRINT'(/"2D-array :")'
        DO I = 1,row
            READ(15,*) (a(I,J), J=1,col)
            WRITE(*,*) a(I,:)
        END DO
    
    CLOSE(15)

    ! 垂直方向的總和&平均
    row_sa = 0 

        DO I = 1,row
            DO J = 1,col
                row_sa(I,1) = row_sa(I,1) + a(I,J)
            END DO
            row_sa(I,2) = row_sa(I,1) / J
        END DO
        
        PRINT'(/"row-direction :")'
        WRITE(*,*)("+-------------------------------------+")
        WRITE(*,*)("|       sum        |       ave        |")
        WRITE(*,*)("+-------------------------------------+")
        DO I = 1,row
            WRITE(*,*) "|",row_sa(I,1),"|",row_sa(I,2),"|"
            WRITE(*,*)("+-------------------------------------+")
        END DO

    DEALLOCATE(row_sa) ! 用完了記得釋放記憶體空間
    
    ! 水平方向的總和&平均
    col_sa = 0
    
        DO J = 1,col
            DO I = 1,row
                col_sa(1,J) = col_sa(1,J) + a(I,J)
            END DO
            col_sa(2,J) = col_sa(1,J) / I
        END DO

        PRINT'(/"column-direction :")'
        WRITE(*,*)"|sum|",col_sa(1,:)
        WRITE(*,*)"|ave|",col_sa(2,:)

    DEALLOCATE(col_sa) ! 用完了記得釋放記憶體空間

    ! 最終，無須再用到陣列a，釋放記憶體空間 
    DEALLOCATE(a) 
    
end program HW08_V3
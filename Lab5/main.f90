real FUNCTION average(n,orderedpairs)

    real total
    integer n
    integer orderedpairs(n)
    real ave
    do i=1,10
       total = total + orderedpairs(i)
    end do

    ave = total/n
    average = ave
    RETURN
    END FUNCTION

real FUNCTION standdev(n,orderedpairs)

    real total2
    real sumofsquares
    real halfway
    real standard
    real aver
    integer n


    integer orderedpairs(n)

    do i=1,10
       total2 = total2 + orderedpairs(i)
    end do

    aver = total2/n

    do i = 1,10
        halfway = ave - orderedpairs(i)
        halfway = halfway ** 2
        sumofsquares = sumofsquares + halfway
    end do

    print *, "Sum of Squares: ",sumofsquares

    standard = SQRT(sumofsquares / (n - 1))
    standdev = standard
    RETURN
    END FUNCTION

Real FUNCTION littleR(n, orderedpairs)

    integer n, i, j
    integer orderedpairs(n,2)
    real average1, average2
    real total3, total4
    real multi
    real keep
    real track
    real sumofsquares1
    real sumofsquares2
    real actualresult
    total3 = 0.0
    total4 = 0.0
    average1 = 0.0
    average2 = 0.0
    keep = 0.0
    track = 0.0
    multi = 0.0
    sumofsquares1 = 0.0
    sumofsquares2 = 0.0
    actualresult = 0.0

    do i=1,10
        total3 = total3 + orderedpairs(i,2)
    end do

    do i=1,10
        j = 1
        total4 = total4 + orderedpairs(i,j)
    end do

    average1 = total3 / 10
    average2 = total4 / 10

    do i = 1,10
        keep = average2 - orderedpairs(i,1)
        track = average1 - orderedpairs(i,2)
        multi = multi + (keep * track)
        sumofsquares1 = sumofsquares1 + (keep ** 2)
        sumofsquares2 = sumofsquares2 + (track ** 2)
    end do

    actualresult = multi / (SQRT((sumofsquares1 * sumofsquares2)))


    littleR = actualresult
    RETURN
    END FUNCTION

program Lab5
    implicit none

    real average
    real ageaverage
    real littleR
    real smallr
    real standdeve
    real standdev
    integer :: orderedpairs(10,2), i, j
    integer :: busage(10)
    integer :: cost(10)
    integer n

    cost = (/350,370,480,520,590,550,750,800,790,950/)
    busage = (/1,2,2,2,2,3,4,4,5,5/)
    n = 10

    do i = 1,10
        orderedpairs(i,1) = busage(i)
        orderedpairs(i,2) = cost(i)
    end do

    ageaverage = average(n,orderedpairs)
    standdeve = standdev(n,orderedpairs)
    smallr = littleR(n,orderedpairs)


    print *, "The Average Age of the Buses is: "
    print *, ageaverage

    print *, "The Sample Standard Deviation for the Age of the Buses: "
    print *, standdeve

    print *, "Little r Is: "
    print *, smallr

end program


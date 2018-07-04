!------------------------------------------------------------------------------
! MODULE: lib
!
!> @author
!> Jakub Czerski
!
! DESCRIPTION:
!> LIBRRARY FOR MATRIX OP.
!
!------------------------------------------------------------------------------

module lib

    implicit none

    contains 

    !------------------------------------------------------------------------------
    !> @author
    !> Jakub Czerski
    !
    ! DESCRIPTION:
    !> MULTIPLICATION OF MATRIXS.
    !
    !>@param[in] first 1st matrix
    !>@param[in] second 2nd matrix
    !>@param[out] multiply res matrix
    !>@param[out] status, 0 on success
    !------------------------------------------------------------------------------

    subroutine mm(first, second, multiply, status)
        
        real(kind = 8),intent(in):: first(:,:) ! pierwsza macierz
        real(kind = 8),intent(in):: second(: ,:) ! druga macierz
        real(kind = 8),intent(out):: multiply(:,:) ! macierz wynikowa
        integer(kind = 4),intent(out):: status ! kod błędu, 0 gdy OK
        integer(kind = 4):: firstW, firstK, secondW, secondK, multiplyW, multiplyK, i, j, ii, jj
        integer(kind = 4)::ichunk

        firstW = SIZE(first(:, 1))
        firstK = SIZE(first(1, :))
        secondW = SIZE(second(:, 1))
        secondK = SIZE(second(1, :))
        multiplyW = SIZE(multiply(:, 1))
        multiplyK = SIZE(multiply(1, :))

        if (firstK .NE. secondW) then
            status = 1
        end if

        ichunk = 512
        do i=1, multiplyK, ichunk
            do j=1, multiplyW, ichunk
                do ii=i, min(i+ichunk-1, multiplyK)
                    do jj=j, min(j+ichunk-1, multiplyW)
                        multiply(ii,jj) = dot_product(first(ii,:), second(:, jj))
                    end do
                end do
            end do
        end do
        
        status = 0

    end subroutine


    !------------------------------------------------------------------------------
    !> @author
    !> Jakub Czerski
    !
    ! DESCRIPTION:
    !> Guess method for matrix.
    !
    !>@param[in] N size of matrix
    !>@param[out] A matrix 
    !>@param[out] X matrix (values)
    !------------------------------------------------------------------------------

    subroutine gauss(A, X, N)

        integer (kind = 4), intent(in) :: N
        real (kind = 4), intent(out) :: A(N,N), X(N)
        integer (kind = 4) :: i, j
        real (kind = 4) :: c

        do i=1, N
            do j=0, N
                if(i .NE. j) then ! i!=j
                    c = A(i, j+1) / A(i, i+1)
                    A(:, j+1) = A(:, j+1) - c * A(:, i+1)
                    X(j+1) = X(j+1) - c * X(i+1)
                end if
            end do
        end do

    end subroutine

end module

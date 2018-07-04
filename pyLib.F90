
subroutine mm(first, second, multiply, AW, AK, BW, BK, status)
	
	integer :: AW, AK, BW, BK
	integer :: first(AW,AK)
	integer :: second(BW,BK)
	integer :: multiply(AW,BK)
	integer(kind = 4),intent(out):: status ! kod błędu, 0 gdy OK
    integer(kind = 4)::i, j, ii, jj
    integer(kind = 4)::ichunk
	!f2py intent(in) :: first, second, AW, AK, BW, BK
	!f2py intent(out) :: multiply

	if (AK .NE. BW) then
		status = 1
	end if

	ichunk = 512
	do i=1, BK, ichunk
		do j=1, AW, ichunk
			do ii=i, min(i+ichunk-1, BK)
				do jj=j, min(j+ichunk-1, AW)
					multiply(ii,jj) = dot_product(first(ii,:), second(:, jj))
				end do
			end do
		end do
	end do
	
	status = 0

end subroutine

subroutine gauss(A, X, N)

	integer, intent(in) :: N
	real, intent(inout) :: A(N,N), X(N)
	integer :: i, j
	real:: c

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

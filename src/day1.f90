subroutine day1(test)
  implicit none
  logical, intent(in) :: test
  integer :: fd, ios, dial, count_part1, count_part2, magnitude, next_position, clicks
  character(80) :: filename
  character(1) :: lr

  dial = 50
  count_part1 = 0
  count_part2 = 0

  if (test) then
    filename = "data/day1/test.txt"
  else
    filename = "data/day1/input.txt"
  end if

  open(unit=fd, file=filename, status="old", action="read")

  do
    read(fd, "(A1,I4)", iostat=ios) lr, magnitude
    if (ios /= 0) then
      exit
    end if
    if (lr == "L") then
      magnitude = -magnitude
    end if
    next_position = dial + magnitude
    clicks = abs(next_position) / 100
    ! If we start from non-zero and end up on a negative number or zero, add an extra click
    if (dial .gt. 0 .and. next_position .le. 0) then
      clicks = clicks + 1
    end if
    count_part2 = count_part2 + clicks
    dial = mod(next_position, 100)
    if (dial .lt. 0) then
      dial = dial + 100
    end if
    if (dial .eq. 0) then
      count_part1 = count_part1 + 1
    end if
    print *, lr, magnitude, next_position, clicks, dial, count_part1, count_part2
  end do

  close(fd)

  print *, "Day 1 Part 1: ", count_part1
  print *, "Day 1 Part 2: ", count_part2

end subroutine day1

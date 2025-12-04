subroutine day3(test)
  implicit none

  logical, intent(in) :: test
  character(80) :: filename
  character(1024) :: buffer
  integer, allocatable :: content(:,:)
  integer :: part1_result(2), part2_result(12)
  integer :: fd = 0, ios, idx, rows, columns, buffer_size, row, column
  integer :: first, second, item
  integer*8 :: count_part1, count_part2, part2_joltage

  if (test) then
    filename = "data/day3/test.txt"
  else
    filename = "data/day3/input.txt"
  end if

  open(unit=fd, file=filename, status="old", action="read")

  ! Scan the file to determine its dimensions
  ios = 0
  rows = 0
  columns = 0
  do while (ios .eq. 0)
    read(fd, *, iostat=ios) buffer
    if (ios .eq. 0) then
      rows = rows + 1
      buffer_size = len(trim(buffer))
      if (buffer_size .gt. columns) columns = buffer_size
    end if
  end do

  ! Create the array and rewind to the start of the file
  allocate(content(rows, columns))
  rewind(fd)

  ! Read the file into an integer array
  do row = 1, rows
    read(fd, *) buffer
    ! print *, trim(buffer)
    do column = 1, columns
      content(row, column) = (ichar(buffer(column:column)) - 48)
    end do
  end do

  close(fd)

  count_part1 = 0
  count_part2 = 0

  ! Get largest two numbers in each row
  do row = 1, rows
    do idx = 1, 2
      part1_result(idx) = 0
    end do
    do idx = 1, 12
      part2_result(idx) = 0
    end do
    do column = 1, columns
      item = content(row, column)
      if (part1_result(2) > part1_result(1)) then
        part1_result(1) = part1_result(2)
        part1_result(2) = 0
      end if
      do idx = 1, 11
        if (part2_result(idx + 1) > part2_result(idx)) then
          part2_result(idx) = part2_result(idx + 1)
          part2_result(idx + 1) = 0
        end if
      end do
      if (item > part1_result(2)) then
        part1_result(2) = item
      end if
      if (item > part2_result(12)) then
        part2_result(12) = item
      end if
    end do
    print *, "1>>", part1_result(:)
    print *, "2>>", part2_result(:)
    count_part1 = count_part1 + (10 * part1_result(1) + part1_result(2))
    part2_joltage = 0
    do idx = 1, 12
      part2_joltage = part2_joltage * 10 + part2_result(idx)
    end do
    count_part2 = count_part2 + part2_joltage
  end do

  print *, "Day 3 Part 1: ", count_part1
  print *, "Day 3 Part 2: ", count_part2

end subroutine day3

subroutine day5(test)
  implicit none

  type range
    integer*8 :: start
    integer*8 :: end
  end type

  logical, intent(in) :: test
  character(80) :: filename
  integer :: fd = 0, ios = 0, element_count = 0, num_ranges = 0, num_values = 0, buffer_size, idx, pos
  ! Completely arbitrary limits on array, hope this works
  character(len=40) :: buffer
  logical :: done = .false.
  type (range), allocatable :: ranges(:)
  type (range), allocatable :: next_ranges(:)
  integer*8, allocatable :: values(:)
  integer*8 :: count_part1, count_part2, value

  if (test) then
    filename = "data/day5/test.txt"
  else
    filename = "data/day5/input.txt"
  end if

  ! Open the input file
  open(unit=fd, file=filename, status="old", action="read")

  ! Scan the file to determine its dimensions
  do while (ios .eq. 0)
    read(fd, *, iostat=ios) buffer
    if (ios .eq. 0) then
      pos = index(buffer, "-")
      if (pos .le. 0) then
        num_values = num_values + 1
      else
        num_ranges = num_ranges + 1
      end if
      print *, buffer, num_ranges, num_values, pos
    end if
  end do

  ! Create the array and rewind to the start of the file
  allocate(ranges(num_ranges))
  allocate(values(num_values))
  rewind(fd)

  ! Read the ranges
  do idx = 1, num_ranges
    read(fd, *) buffer
    pos = index(buffer, "-")
    read (buffer(1:pos-1), *) ranges(idx)%start
    read (buffer(pos+1:), *) ranges(idx)%end
    print *, idx, buffer, pos, ranges(idx)%start, ranges(idx)%end
  end do

  ! Read the rest of the file
  do idx = 1, num_values
    read(fd, *) values(idx)
    print *, idx, values(idx)
  end do

  ! Close the input file
  close(fd)

  ! Initialise the Part 1 and Part 2 values
  count_part1 = 0
  count_part2 = 0

  ! Solve Part 1
  do idx = 1, num_values
    done = .false.
    pos = 1
    do while (pos <= num_ranges .and. .not. done)
      done = (values(idx) .ge. ranges(pos)%start .and. values(idx) .le. ranges(pos)%end)
      pos = pos + 1
    end do
    if (done) count_part1 = count_part1 + 1
  end do

  ! Solve Part 2 - merge ranges
  done = .false.
  do while (.not. done)
    done = .true.
    do idx = 1, num_ranges - 1
      do pos = idx + 1, num_ranges
        if (ranges(idx)%end .ne. 0 .and. ranges(idx)%start .le. ranges(pos)%end .and. ranges(pos)%start .le. ranges(idx)%end) then
          print *, "MERGING:", idx, pos, ranges(idx)%start, ranges(idx)%end, ranges(pos)%start, ranges(pos)%end
          ranges(idx)%start = min(ranges(idx)%start, ranges(pos)%start)
          ranges(idx)%end = max(ranges(idx)%end, ranges(pos)%end)
          ranges(pos)%start = 0
          ranges(pos)%end = 0
          print *, "MERGED:", idx, pos, ranges(idx)%start, ranges(idx)%end
          done = .false.
        end if
      end do
    end do
  end do

  ! Solve Part 2 - count range sizes
  do idx = 1, num_ranges
    if (ranges(idx)%end .ne. 0) then
      count_part2 = count_part2 + 1 + ranges(idx)%end - ranges(idx)%start
    end if
  end do

  ! Output the results
  print *, "Day 5 Part 1: ", count_part1
  print *, "Day 5 Part 2: ", count_part2

end subroutine day5

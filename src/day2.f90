subroutine day2(test)
  implicit none

  type range
    integer*8 :: start
    integer*8 :: end
  end type

  logical, intent(in) :: test
  logical :: is_invalid
  integer :: fd = 0, ios, element_count, idx, pos, num_digits
  integer*8 :: count_part1, count_part2, product_id, dividend
  character(80) :: filename
  ! Completely arbitrary limits on array, hope this works
  character(len=24), dimension(1000) :: ranges_str
  type (range), allocatable :: ranges(:)
  type (range) :: logs
  integer*8 :: invalid_divisors(9,3)
  integer :: invalid_part1_divisors(9)

  invalid_divisors = reshape( (/ &
  ! 2.. 3.... 4.... 5...... 6...... 7........ 8........ 9.......... 10......... digits
    11, 111,  1111, 11111,  111111, 1111111,  11111111, 111111111,  1111111111, &
    0,  0,    101,  0,      10101,  0,        1010101,  1001001,    101010101,  &
    0,  0,    0,    0,      1001,   0,        10001,    0,          100001      /), shape(invalid_divisors))
  invalid_part1_divisors = [1, 3, 2, 3, 3, 3, 3, 3, 3]

  if (test) then
    filename = "data/day2/test.txt"
  else
    filename = "data/day2/input.txt"
  end if

  open(unit=fd, file=filename, status="old", action="read")

  ! From stack overflow, this is a bit gross
  element_count = 0
  ios = 0
  do while (ios .eq. 0)
    read(fd, *, iostat=ios) ranges_str(1:element_count+1)
    if (ios .eq. 0) then
      element_count = element_count + 1
      rewind(fd)
    end if
  end do

  rewind(fd)
  read(fd, *) ranges_str(1:element_count)

  close(fd)

  allocate(ranges(element_count))
  do idx = 1, element_count
    pos = index(ranges_str(idx), "-")
    read (ranges_str(idx)(1:pos-1), *) ranges(idx)%start
    read (ranges_str(idx)(pos+1:), *) ranges(idx)%end
  end do

  count_part1 = 0
  count_part2 = 0

  ! Given a set of ranges, find the number of invalid IDs
  do idx = 1, element_count
    do product_id = ranges(idx)%start, ranges(idx)%end
      ! Cast product_id to double-precision real or 9,999,999 will round up to 10,000,000 :-/
      num_digits = floor(log10(real(product_id, kind(1.0d0))))

      ! Part One
      dividend = invalid_divisors(num_digits, invalid_part1_divisors(num_digits))
      is_invalid = (dividend .ne. 0 .and. mod(product_id, dividend) .eq. 0)
      if (is_invalid) then
        count_part1 = count_part1 + product_id
        ! print *, "1>>", product_id, count_part1
      end if

      ! Part Two
      is_invalid = .false.
      do pos = 1, 3
        dividend = invalid_divisors(num_digits, pos)
        is_invalid = (is_invalid .or. (dividend .ne. 0 .and. mod(product_id, dividend) .eq. 0))
      end do
      if (is_invalid) then
        count_part2 = count_part2 + product_id
        print *, "2>>", ranges(idx)%start, ranges(idx)%end, product_id, num_digits, pos, count_part2
      end if
    end do
  end do

  print *, "Day 2 Part 1: ", count_part1
  print *, "Day 2 Part 2: ", count_part2

end subroutine day2

module day4_fn
  implicit none
  contains
  logical function grid_value(grid, row, column, rows, columns)
    logical, intent(in) :: grid(:,:)
    integer, intent(in) :: row, column, rows, columns
    logical :: value

    value = (row >= 1 .and. row <= rows .and. column >= 1 .and. column <= columns .and. grid(row, column))
  end function grid_value
end module day4_fn

subroutine day4(test)
  use day4_fn
  implicit none

  logical, intent(in) :: test
  character(80) :: filename
  character(1024) :: buffer
  logical, allocatable :: grid(:,:), next_grid(:,:)
  integer :: fd = 0, ios, idx, rows, columns, buffer_size, row, column, count, x, y
  integer*8 :: count_part1, running_count, count_part2
  integer :: directions(8,2)
  logical :: done_once
  
  directions = reshape( (/ &
  ! N   NE  E   SE  S   SW  W   NW
    -1, -1, 0,  1,  1,  1,  0,  -1, &
    0,  1,  1,  1,  0,  -1, -1, -1 /), shape(directions))

  if (test) then
    filename = "data/day4/test.txt"
  else
    filename = "data/day4/input.txt"
  end if

  ! Open the input file
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
  allocate(grid(rows, columns))
  allocate(next_grid(rows, columns))
  rewind(fd)

  ! Read the file into a logical array
  do row = 1, rows
    read(fd, *) buffer
    ! print *, trim(buffer)
    do column = 1, columns
      grid(row, column) = (buffer(column:column) == "@")
    end do
    ! print *, row, grid(row,:)
  end do

  ! Close the input file
  close(fd)

  ! Initialise the Part 1 and Part 2 values
  count_part1 = 0
  count_part2 = 0

  ! Work out which locations have fewer than four adjacent markers
  done_once = .false.
  do
    running_count = 0
    do row = 1, rows
      do column = 1, columns
        next_grid(row, column) = .false.
        if (grid(row, column)) then
          count = 0
          do idx = 1, 8 ! Compass directions
            y = row + directions(idx, 1)
            x = column + directions(idx, 2)
            if (grid_value(grid, y, x, rows, columns)) count = count + 1
          end do
          ! If there are fewer than four adjacent occupied locations, mark them
          if (count < 4) then
            running_count = running_count + 1
          else
            next_grid(row, column) = .true.
          end if
        end if
      end do
    end do
    if (.not. done_once) then
      count_part1 = running_count
      done_once = .true.
    end if
    count_part2 = count_part2 + running_count
    if (running_count .eq. 0) exit
    do row = 1, rows
      do column = 1, columns
        grid(row, column) = next_grid(row, column)
      end do
    end do
  end do

  ! Output the results
  print *, "Day 4 Part 1: ", count_part1
  print *, "Day 4 Part 2: ", count_part2

end subroutine day4

program make_rank
  implicit none

  ! #########################################################################
  ! ### DECLARATION SECTION

  ! --- Parameters ---
  ! Word length, ifort(assume-byterecl):1, else:4
  integer, parameter :: WRD_LEN = 4
  ! temporal file unit id
  integer, parameter :: TMP_UID = 21

  ! --- Project Props
  integer, parameter :: MX = 87
  integer, parameter :: MY = 95
  ! character, parameter :: CNAME*8 = 'th-gsmap'
  ! character, parameter :: CRESL*6 = '0.1deg'
  ! real, parameter :: XMIN = 97.20
  ! real, parameter :: XMAX = 105.90
  ! real, parameter :: RESL = 0.1
  ! integer, parameter :: ISY = 2001
  ! integer, parameter :: IEY = 2001

  ! --- Data File Paths ---
  ! Data depot directory Path
  character(128), parameter :: DEPOT_DIR = '/home/tatsuzaki98/RWES/water-demand-ratio/depot/'
  character(128) :: map_dir = trim(DEPOT_DIR)//'/river_map'
  character(128) :: output_dir = trim(DEPOT_DIR)//'/river_map'

  ! --- Variables ---
  integer rank(mx, my, 3) !1:rivre number 2:from estuary 3:branch number
  integer jx(mx, my), jy(mx, my) !Next XY
  real uparea(mx, my)
  real catchment(mx, my)
  integer ibasin
  logical bchange
  integer id !distance from estuary
  integer, allocatable :: id_max(:)
  logical bbranch(mx, my)
  integer i, j, k, ii, jj
  integer ic !count
  integer ib !branch
  integer ib_max
  integer ib_max_temp

  !======================================================================
  open (TMP_UID, file=trim(map_dir)//'/nextxy.bin', &
    & form='unformatted', access='direct', recl=mx*my*WRD_LEN, action='read')
  read (TMP_UID, rec=1) ((jx(i, j), i=1, mx), j=my, 1, -1) !Read as South to North
  read (TMP_UID, rec=2) ((jy(i, j), i=1, mx), j=my, 1, -1)
  close (TMP_UID)

  do j = 1, my
    do i = 1, mx
      if (jy(i, j) >= 1) jy(i, j) = my - jy(i, j) + 1 !North to South -> South to North
    end do
  end do

  open (TMP_UID, file=trim(map_dir)//'/uparea.bin', &
    & form='unformatted', access='direct', recl=mx*my*WRD_LEN, action='read')
  read (TMP_UID, rec=1) ((uparea(i, j), i=1, mx), j=my, 1, -1)
  close (TMP_UID)

  rank (:, :, :) = 0

  ! 1. find river mouth
  ibasin = 0
  do j = 1, my
  do i = 1, mx
    if (jx(i, j) == -9 .and. jy(i, j) == -9) then        !Estuary
      ibasin = ibasin + 1
      rank (i, j, 1) = ibasin
      rank (i, j, 2) = 1
      rank (i, j, 3) = 1
    else if (jx(i, j) == -10 .and. jy(i, j) == -10) then !In land edge
      ibasin = ibasin + 1
      rank (i, j, 1) = ibasin
      rank (i, j, 2) = 1
      rank (i, j, 3) = 1
    end if
  end do
  end do
  write (6, *) 'ibasin_max=', ibasin

  ! 2. make Rank1:rivr number and Rank2:distance from the river mouth
  allocate (id_max(ibasin))
  id_max(:) = 1
  id = 1
  bchange = .true.
  do while (bchange)
    id = id + 1; bchange = .false.
    do j = 1, my
    do i = 1, mx
      if (jx(i, j) <= -9 .or. jy(i, j) <= -9) cycle !end of the rivers
      if (rank(jx(i, j), jy(i, j), 2) == id - 1) then
        rank (i, j, 1) = rank(jx(i, j), jy(i, j), 1)
        rank (i, j, 2) = id
        bchange = .true.
        id_max(rank(jx(i, j), jy(i, j), 1)) = id
      end if
    end do
    end do
  end do

  ! 3. make Rank3:branch number
  do k = 1, ibasin
    if (id_max(k) == 1) cycle ! 1grid river
    ib_max = 1; ib_max_temp = 0
    do id = 2, id_max(k) !river mouth -> river source
      bbranch(:, :) = .false.; ic = 0
      do j = 1, my
      do i = 1, mx
        if (rank(i, j, 2) == id .and. rank(i, j, 1) == k) then
          bbranch(i, j) = .true.   !not the river source
          ic = ic + 1             !count branches
        end if
      end do
      end do

      if (ic >= 2) then  !the distance has brances
        do j = 1, my
        do i = 1, mx
          if (bbranch(i, j)) then
            ib = 0
            do jj = 1, my
            do ii = 1, mx
              if (i == ii .and. j == jj) cycle
              if (bbranch(ii, jj)) then
                if (uparea(i, j) < uparea(ii, jj) .and. jx(i, j) == jx(ii, jj) .and. jy(i, j) == jy(ii, jj)) then
                  ib = ib + 1
                else if (uparea(i, j) == uparea(ii, jj)) then
                  write (6, *) 'the same uparea', i, j, ii, jj
                  stop
                end if
              end if
            end do
            end do
            if (ib == 0) then !the only upstream or the main stream
              rank (i, j, 3) = rank(jx(i, j), jy(i, j), 3)
            else
              rank (i, j, 3) = ib_max + ib
              ib_max_temp = max(ib_max_temp, ib_max + ib)
            end if
          end if
        end do
        end do
      else !the only upstream
        do j = 1, my
        do i = 1, mx
          if (bbranch(i, j)) rank(i, j, 3) = rank(jx(i, j), jy(i, j), 3)
        end do
        end do
      end if
      ib_max = max(ib_max, ib_max_temp)
      write (6, *) ic, ib_max, k
    end do
  end do

  do j = my, 1, -1
    do i = 1, mx - 1
      write (6, '(i3)', advance='no') rank(i, j, 3)
    end do
    write (6, '(i3)') rank(mx, j, 3)
  end do

  do j = 1, my
    do i = 1, mx
      if (uparea(i, j) >= 0.e0) then
        catchment(i, j) = uparea(i, j)
      else
        catchment(i, j) = 0.e0
      end if
    end do
  end do

  ! ########################################################################
  ! ### OUTPUT SECTION

  open (TMP_UID, file=trim(output_dir)//'/rank.bin',&
    &form='unformatted', access='direct', recl=mx*my*WRD_LEN, status='replace')
  write (TMP_UID, rec=1) ((rank(i, j, 1), i=1, mx), j=my, 1, -1) !Write as North to South
  write (TMP_UID, rec=2) ((rank(i, j, 2), i=1, mx), j=my, 1, -1)
  write (TMP_UID, rec=3) ((rank(i, j, 3), i=1, mx), j=my, 1, -1)
  close (TMP_UID)

  open (TMP_UID, file=trim(output_dir)//'/nextxy.bin', &
    & form='unformatted', access='direct', recl=mx*my*WRD_LEN, status='replace')
  write (TMP_UID, rec=1) ((jx(i, j), i=1, mx), j=my, 1, -1) !Write as North to South
  write (TMP_UID, rec=2) ((jy(i, j), i=1, mx), j=my, 1, -1) !Write as North to South
  close (TMP_UID)

  open (TMP_UID, file=trim(output_dir)//'/catchment-area.bin', &
    & form='unformatted', access='direct', recl=mx*my*WRD_LEN, status='replace')
  write (TMP_UID, rec=1) ((catchment(i, j), i=1, mx), j=my, 1, -1) !Write as North to South
  close (TMP_UID)

end program make_rank

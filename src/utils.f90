module utils
  implicit none
  private

  public :: calc_Ademand, make_getgrid, make_urbanWL, comp_reverse, &
    & comp_jump, urbanWL, path_1g, get_monday, get_term, writer

  contains
  !======================================================================
  !++ calc_DIdemand
  !++ calc_Ademand
  !++ make_getgrid
  !++ make_urbanWL
  !++ comp_reverse(outflw)
  !++ comp_jump(outflw)
  !++ urbanWL
  !++ path_1g
  !++ get_monday(iy,days)
  !++ get_term(iy,day,t)
  !======================================================================

  ! ############################################################################
  ! # Procedure Description Here
  ! # 
  pure function calc_Ademand() result(wd_a)
    use ctrl_vars , only : mx, my, area, paddy, dd, we

    ! --- returns
    real wd_a(mx,my)
    integer i , j

    do j = 1 , my
      do i = 1 , mx
        if( paddy(i,j) > 0.e0 )then
          wd_A(i,j) = area(i,j) * paddy(i,j) * dd * 1.e-3 / we
        else
          wd_A(i,j) = 0.e0  
        end if
      end do
    end do
  end function calc_Ademand


  !======================================================================
  subroutine make_getgrid
  use ctrl_vars , only:ibin,isy,iey,mx,my,jx,jy,ig,jg,bjump,rank,outflw_path
  real outflw(mx,my)
  real aoutflw(mx,my)  !Annual OutFlow
  integer ides(mx,my)   !descending order of aoutflw, 1:maximum flow , -99:no river
  integer ides_min
  integer ic
  logical bpath !possibility of path
  integer inxt , jnxt
  integer inxt_temp , jnxt_temp
  integer irn !rank(i,j,3) of its down stream, Ineger/Rank/Next
  integer iy , id , days
  character(4) cyear
  integer k
  integer ii , jj
    integer i , j
  !----------------------------------------------------------------------
  aoutflw(:,:) = 0.e0
  do iy = isy , iey
    call get_monday(iy,days)
    write(cyear,'(i4)') iy
    open(104,file=trim(outflw_path)//'outflw'//cyear//'.bin',&
  &form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
    do id = 1 , days
      read(104,rec=id) ((outflw(i,j),i=1,mx),j=my,1,-1)
      do j = 1 , my
      do i = 1 , mx
        if( outflw(i,j) <= 1.e6 )then
          aoutflw(i,j) = aoutflw(i,j) + outflw(i,j) / ( real(id) * real( iey - isy + 1 ) )
        end if
      end do
      end do
    end do
  end do

  if( bjump ) call comp_jump(aoutflw)

  ides(:,:) = -99
  do j = 1 , my
  do i = 1 , mx
    if( aoutflw(i,j) == 0.e0 ) cycle !no river = -99
    ic = 1
    do jj = 1 , my
    do ii = 1 , mx
      if( i == ii .and. j == jj ) cycle
      if( aoutflw(i,j) < aoutflw(ii,jj) ) ic = ic + 1
    end do
    end do 
    ides(i,j) = ic
  end do
  end do  

  !REQUIREMENTS
  ! 1. the other basin
  ! 2. the same basin, but don't flow into (mx,my)
  ! 3. the same basin, but the main stream or the branch separated somewhere at downstream
  ! 4. the same baisn, but the distance to the river mouth is more than 5 gied shorter than that of (mx,my)
  ! 5. reclaimed land


  do j = 1 , my
  do i = 1 , mx
    ides_min = mx * my + 1
    ig(i,j) = -9 ; jg(i,j) = -9
    do jj = max(j-1,1) , min(j+1,my) !around 1 grid cells
    do ii = max(i-1,1) , min(i+1,mx) !around 1 grid cells
      if( ii == i .and. jj == j ) cycle !avoid itself
      if( ides(ii,jj) < 0 ) cycle !no river
      bpath = .false.
      if( rank(ii,jj,1) /= rank(i,j,1) ) bpath = .true. !REQUIREMENT 1
      if( rank(ii,jj,1) == rank(i,j,1) .and. rank(ii,jj,3) /= rank(i,j,3) )then
        if( rank(ii,jj,3) .lt. rank(i,j,3) )then !(ii,jj) is the main stream or down stream
          bpath = .true. !REQUIREMENT 3
        else !check whether (ii,jj) will flow into (i,j)
          irn = mx * my + 1
          inxt = jx(ii,jj) ; jnxt = jy(ii,jj)
          if( inxt <= -9 .or. jnxt <= -9 ) cycle !one grid river   
          do k = 1 , 3
            if( inxt <= -9 .or. jnxt <= -9 ) exit
            irn = min( irn , rank(inxt,jnxt,3) )
            if( irn < rank(i,j,3) ) exit
            inxt_temp = inxt ; jnxt_temp = jnxt
            inxt = jx(inxt_temp,jnxt_temp) ; jnxt = jy(inxt_temp,jnxt_temp)
          end do  
          if( irn < rank(i,j,3) ) bpath = .true. !REQUIREMENT 2
        end if 
      end if 
      if( rank(ii,jj,2) >= rank(i,j,2) + 5 ) bpath = .true. !REQUIREMENT 4 ; far from (i,j)
      if( aoutflw(i,j) == 0.e0 ) bpath = .true. !REQUIREMENT 5 ; reclaimed land

      if( .not. bpath ) cycle
      if( ides(ii,jj) .lt. ides_min .and. aoutflw(ii,jj) .gt. 0.e0 )then
        ides_min = ides(ii,jj)
        ig(i,j) = ii ; jg(i,j) = jj !Choose max annual flow grid cell
      end if
    
    end do
    end do
  end do
  end do

  return
  end subroutine make_getgrid
  !======================================================================
  subroutine comp_reverse(outflw)
  use ctrl_vars , only : mx , my , jx , jy
  real,intent(inout) :: outflw(mx,my)
  real outflw_temp(mx,my)
  real inflw
  integer ii , jj
    integer i , j

  do j = 1 , my
  do i = 1 , mx
    outflw_temp(i,j) = outflw(i,j)
  end do
  end do

  do j = 1 , my
  do i = 1 , mx
    if( outflw_temp(i,j) < 0.e0 )then
      inflw = 0.e0  
      do jj = 1 , my
      do ii = 1 , mx
        if( jx(ii,jj) == i .and. jy(ii,jj) == j )then
          inflw = inflw + outflw_temp(ii,jj)
        end if
      end do
      end do
      if( inflw > 0.e0 )then
        outflw(i,j) = inflw
      end if 
    end if 
  end do
  end do

  return
  end subroutine comp_reverse
  !======================================================================
  subroutine make_urbanWL
  use ctrl_vars , only: mx, my, pref, iuwl
  integer i , j

  ! Case1. Yodo(23,17) -> Osaka & East Hyogo
  do j = 1 , my
  do i = 1 , mx
    if( pref(i,j) == 27 )then
      iuwl(i,j) = 1
    else if( pref(i,j) == 28 .and. i >= 11 .and. j >= 13 .and. j <= 18)then
      iuwl(i,j) = 1
    end if
  end do
  end do      

  write(6,*) 'Urban Water Line'
  do j = my , 1 , -1
    do i = 1 , mx
      write(6,'(i1)',advance='no') iuwl(i,j)
    end do
    write(6,'(a1)') ' '
  end do 

  return
  end subroutine make_urbanWL
  !======================================================================
  subroutine comp_jump(outflw)
  use ctrl_vars , only : mx,my,jx,jy
  real,intent(inout) :: outflw(mx,my)
  real outflw_temp(mx,my)
  real a            ! river line => (Y-y) = a(X-x)
  real xx , yy      ! substitute [x + N*d, N=1,2,...] for (Y-y) = a(X-x)
  integer ixx , iyy ! integer(xx) integer(yy)
  real,parameter :: d = 1.e-1
  integer n
  integer,parameter :: n_max = 90 !max jump distance = 9 / d because max(jx&jy) = 8
  logical bcomp(mx,my)
  integer i , j

  do j = 1 , my
  do i = 1 , mx
    outflw_temp(i,j) = outflw(i,j)
  end do
  end do

  do j = 1 , my
  do i = 1 , mx
    if( jx(i,j) <= -9 .and. jy(i,j) <= -9 ) cycle !the river mouth or inland termination
    
    if( i .eq. jx(i,j) .and. abs(j-jy(i,j)) .gt. 1 )then      !Vertical Jump
      do n = min( j , jy(i,j) ) + 1 , max( j , jy(i,j) ) - 1
        outflw(i,n) = outflw(i,n) + outflw_temp(i,j)
      end do
    
    else if( abs(i-jx(i,j)) .gt. 1 .and. j .eq. jy(i,j) )then !Horizontal Jump
      do n = min( i , jx(i,j) ) + 1 , max( i , jx(i,j) ) - 1
        outflw(n,j) = outflw(n,j) + outflw_temp(i,j)
      end do

    else if( abs(i-jx(i,j)) .gt. 1 .or. abs(j-jy(i,j)) .gt. 1 )then !Slanted Jump
      a = real( jy(i,j) - j ) / real( jx(i,j) - i )    
      ! river line function => (Y-j) = a(X-i)

      bcomp(:,:) = .false. 
      bcomp(i,j) = .true.              !need not to consider JUMP at both edges
      bcomp(jx(i,j),jy(i,j)) = .true.  !need not to consider JUMP at both edges

      do n = 1 , int( 100.e0 / d )
        xx = real( min( i , jx(i,j) ) ) + real(n) * d
        if( xx .gt. real( max( i , jx(i,j) ) ) ) exit
        yy = a * ( xx - real(i) ) + real(j)

        if( xx - int(xx) .le. 5.e-1 )then ! xx = I.0 ~ I.5
          ixx = int(xx)
        else                              ! xx = I.5 ~ I.999...
          ixx = int(xx) + 1               ! ==ceiling(xx)
        end if
    
        if( yy - int(yy) .le. 5.e-1 )then ! yy = I.0 ~ I.5
          iyy = int(yy)
        else                              ! yy = I.5 ~ I.999...
          iyy = int(yy) + 1               ! ==ceiling(yy)
        end if

        if( bcomp(ixx,iyy) ) cycle !already complemented
        bcomp(ixx,iyy) = .true. 

        outflw(ixx,iyy) = outflw(ixx,iyy) + outflw_temp(i,j)
      end do !n
      if( n > n_max )then
        write(6,*) 'long jump happened, i , j , jx(i,j) , jy(i,j) = ' , i , j , jx(i,j) , jy(i,j)
      end if  

    end if !3 jump patterns
  end do
  end do 

  return
  end subroutine comp_jump
  !=====================================================================-
  subroutine urbanWL(wd,wr,wl)
  use ctrl_vars , only : mx,my,rank,iuwl
  real,intent(in) :: wd(mx,my)
  real,intent(inout) :: wr(mx,my) , wl(mx,my)
  real wro(mx,my) !water resource from its branch
  real twl !total water lack
    integer i , j

  wro(:,:) = 0.e0
  ! Case 1 : Yodo River(23,17) -> Osaka & East Hyogo
  wr(23,17) = wr(23,17) - 80.e0 * 86400.e0 !Kanzaki & Old Yodo
  twl = wd(23,17)
  do j = 1 , my
  do i = 1 , mx  
    if( iuwl(i,j) == 1 .and. rank(i,j,1) == rank(23,17,1) .and. rank(i,j,3) == rank(23,17,3))then
      wro(i,j) = max( wr(i,j)-wr(23,17) , 0.e0 ) 
      twl = twl + wd(i,j) - wro(i,j) !downstream of (23,17)
    else if( iuwl(i,j) == 1 )then
      twl = twl + wl(i,j)
    end if 
  end do
  end do
  
  if( wr(23,17) >= twl )then  !Enough Water
    do j = 1 , my
    do i = 1 , mx
      if( iuwl(i,j) == 1 .and. rank(i,j,1) == rank(23,17,1) .and. rank(i,j,3) == rank(23,17,3))then
        wr(i,j) = wd(i,j)
        wl(i,j) = twl - wr(23,17) - wro(i,j)
      else if( iuwl(i,j) == 1 )then
        wr(i,j) = wd(i,j)
        wl(i,j) = 0.e0
      end if
    end do
    end do
  else   !Water Lack
    do j = 1 , my
    do i = 1 , mx
      if( iuwl(i,j) == 1 .and. rank(i,j,1) == rank(23,17,1) .and. rank(i,j,3) == rank(23,17,3))then
        wr(i,j) = wd(i,j) * wr(23,17) / twl + wro(i,j)
        wl(i,j) = 0.e0
      else if( iuwl(i,j) == 1 )then
        wr(i,j) = wd(i,j) * wr(23,17) / twl
        wl(i,j) = wd(i,j) - wr(i,j)
      end if
    end do
    end do 
  end if

  return
  end subroutine urbanWL
  !======================================================================
  subroutine path_1g(wd,wr,wl)
  use ctrl_vars , only : mx,my,ig,jg
  real,intent(in) :: wd(mx,my)
  real,intent(inout) :: wr(mx,my) , wl(mx,my)
  real wl_temp(mx,my)
    integer i , j

  do j = 1 , my
  do i = 1 , mx
    wl_temp(i,j) = wl(i,j)
  end do
  end do  

  ! 1st tern : receave from (ig,jg) grid freely
  do j = 1 , my
  do i = 1 , mx
    if( wl(i,j) <= 0.e0 ) cycle !surplus
    if( ig(i,j) == -9 .or. jg(i,j) == -9 ) cycle
    if( wl(ig(i,j),jg(i,j)) >= 0.e0 ) cycle !can't borrow
    wl_temp(ig(i,j),jg(i,j)) = wl_temp(ig(i,j),jg(i,j)) + wl(i,j)
  end do
  end do

  ! 2nd : check whether (ig,jg) has surplus
  do j = 1 , my
  do i = 1 , mx
    if( wl(i,j) .le. 0.e0 ) cycle
    if( ig(i,j) .eq. -9 .or. jg(i,j) .eq. -9 ) cycle
    if( wl(ig(i,j),jg(i,j)) .ge. 0.e0 ) cycle !can't borrow
    if( wl_temp(ig(i,j),jg(i,j)) .le. 0.e0 )then !(ig,jg) has surplus
      wl(i,j) = 0.e0
    else
      wl(i,j) = wl(i,j) * ( -1.e0 * wl(ig(i,j),jg(i,j)) ) &
  &           / ( -1.e0 * wl(ig(i,j),jg(i,j))  + wl_temp(ig(i,j),jg(i,j)) ) !require /total require
    end if
  end do
  end do  

  ! 3rd : reflect wl to wr
  do j = 1 , my
  do i = 1 , mx
    if( wl(i,j) < wl_temp(i,j) ) wr(i,j) = wd(i,j) - wl(i,j)
  end do
  end do

  return
  end subroutine path_1g
  !======================================================================
  subroutine get_monday(iy,days)
  integer,intent(in) ::  iy
  integer,intent(out) :: days
  integer leap
      
  leap=0
  if( mod(iy,4) == 0 ) leap=1
  if( mod(iy,100) == 0 .and. mod(iy,400) /= 0 ) leap=0
  if( leap == 0 )then
    days=365   
  else
    days=366   
  end if
  end subroutine get_monday
  !======================================================================
  subroutine get_term(iy,day,t)
  integer,intent(in) ::  iy , day
  integer,intent(out) :: t
  integer leap
  integer doy(13)
  integer im , id

  leap=0
  if( mod(iy,4) == 0 ) leap=1
  if( mod(iy,100) == 0 .and. mod(iy,400) /= 0 ) leap=0
  if( leap == 0 )then
    doy(1:13)=(/0,31,59,90,120,151,181,212,243,273,304,334,365/)
  else
    doy(1:13)=(/0,31,60,91,121,152,182,213,244,274,305,335,366/)
  end if

  do im = 1 , 12
    if( day <= doy(im+1) )then
      id = day - doy(im)
      select case(id)
        case(:10)   ; t = (im-1) * 3 + 1
        case(11:20) ; t = (im-1) * 3 + 2
        case(21:)   ; t = (im-1) * 3 + 3
        case default
          write(6,*) 'ERROR get_term, id'
          stop
      end select
      exit
    end if
  end do
  if( im == 13 )then
    write(6,*) 'ERROR get_term, im'
    stop
  end if  


  return
  end subroutine get_term

    
  !======================================================================
  subroutine writer(iy,cwd)
    use ctrl_vars , only: mx,my,mask,csave,suffix,isy,iey, ibin
    implicit none
    integer,intent(in) :: iy  ! 9999 -> total
    real,intent(in) :: cwd(mx,my,2)
    real cwdw(mx,my) !cwd_write
    integer i , j
    character(4) cyear
    character(9) cterm

    do j = 1 , my
    do i = 1 , mx
      if( mask(i,j) < 0.5)then           !Out of the mask
        cwdw(i,j) = -9999.
      else if( cwd(i,j,2) == 0.e0 )then  !No water demnd
        cwdw(i,j) = -5555.
      else
        cwdw(i,j) = cwd(i,j,1) / cwd(i,j,2)
        if( cwdw(i,j) < 0.9 .and. iy == 9999 ) write(6,'(i4,i4,f7.3)') i , j , cwdw(i,j)
      end if
    end do
    end do

    if( iy /= 9999 )then
      write(cyear,'(i4)') iy
      open(31,file='./output/CWD_'//trim(suffix)//'_'//trim(csave)//'_'//cyear//'.bin',&
    & form='unformatted', access='direct', recl=mx*my*ibin, status='replace')
    else
      write(cterm,'(i4,a1,i4)') isy, '-', iey
      open(31,file='./output/CWD_'//trim(suffix)//'_'//trim(csave)//'_'//cterm//'.bin',&
    & form='unformatted', access='direct', recl=mx*my*ibin, status='replace')
    end if

    write(31,rec=1) ((cwdw(i,j),i=1,mx),j=1,my)
    close(31)

    return
  end subroutine writer


end module utils

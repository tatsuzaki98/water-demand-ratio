subroutine urbanWL(wd,wr,wl)
use ctrl_vars , only : mx,my,rank,iuwl,bjump
integer,parameter :: lx = 23 , ly = 17 !water intake point
real,intent(inout) :: wd(mx,my)
real,intent(inout) :: wr(mx,my) , wl(mx,my)
real wro(mx,my) !water resource from its branch, Water Resource Origin
real twl !total water lack
real kc(mx,my) !Kanzaki & Old Yodo
real ws(mx,my) ! water surplus after distribution
real wl2       ! water lack rate to distribute
real rnof

!+++++ Case 1 : Yodo River(lx,ly) -> Osaka & East Hyogo +++++

rnof = wr(lx,ly)
! 1st step : consider Kanzaki & Old Yodo Channel
!kc(:,:) = 0.e0
!kc(lx,ly) = -80.e0 * 86400.e0
!do j = 1 , my
!do i = 1 , mx
!  if( rank(i,j,1) == rank(lx,ly,1) .and. rank(i,j,2) < rank(lx,ly,2) .and. rank(i,j,3) == rank(lx,ly,3) )then
!    kc(i,j) = -80.e0 * 86400.e0
!  end if
!end do
!end do
!if( bjump ) call comp_jump(kc)
!
!do j = 1 , my
!do i = 1 , mx
!  wd(i,j) = wd(i,j) - kc(i,j)
!  wl(i,j) = wl(i,j) - kc(i,j)
!end do
!end do
!write(6,*) 'Before', wr(19,14) , wl(19,14) , wd(14,19)
! 2nd step : divide WR -> through (lx,ly) or not
wro(:,:) = 0.e0
wro(lx,ly) = wr(lx,ly)
do j = 1 , my
do i = 1 , mx
  if( rank(i,j,1) == rank(lx,ly,1) .and. rank(i,j,2) < rank(lx,ly,2) .and. rank(i,j,3) == rank(lx,ly,3) )then
    wro(i,j) = wr(lx,ly)
  end if
end do
end do
if( bjump ) call comp_jump(wro)

do j = 1 , my
do i = 1 , mx
  if( iuwl(i,j) == 1 )then
    wro(i,j) = max( wr(i,j) - wro(i,j) , 0.e0 ) ! WR which is not through (lx,ly)
    wl(i,j) = wl(i,j) + ( wr(i,j) - wro(i,j) )  ! recalculate wl
  end if  
end do
end do

! 3rd step : estimate Water Lack
twl = wd(lx,ly) + 80.e0 * 86400.e0
do j = 1 , my
do i = 1 , mx  
  if( iuwl(i,j) == 1 .and. wl(i,j) > 0.e0 )then
    twl = twl + wl(i,j)
  end if 
end do
end do
 
if( wr(lx,ly) >= twl )then  !Enough Water
  ws(:,:) = 0.e0
  ws(lx,ly) = twl - wr(lx,ly)
  do j = 1 , my
  do i = 1 , mx
    if( rank(i,j,1) == rank(lx,ly,1) .and. rank(i,j,2) < rank(lx,ly,2) .and. rank(i,j,3) == rank(lx,ly,3) )then
      ws(i,j) = wr(lx,ly) - twl
    end if
  end do
  end do
  if( bjump ) call comp_jump(ws)

  do j = 1 , my
  do i = 1 , mx
    if( iuwl(i,j) == 1 )then
      wr(i,j) = max( wd(i,j) , wr(i,j) )
      if( wl(i,j) > 0.e0 )then ! receave from Urban WL
        wl(i,j) = 0.e0
      else
        wl(i,j) = wl(i,j) - ws(i,j)
      end if
    end if
  end do
  end do

else   !Water Lack
  wl2 = ( wr(lx,ly) - 80.e0 * 86400.e0 ) / ( twl - 80.e0 * 86400.e0 )
  if( wl2 < 0.e0 ) wl2 = 0.e0
  write(6,*) 'Water Lack at UWL, ' , wl2 , rnof / 86400. , twl / 86400.
  do j = 1 , my
  do i = 1 , mx
    if( iuwl(i,j) == 1 )then
      if( wl(i,j) > 0.e0 )then ! receave from Urban WL
        wl(i,j) = wl(i,j) * ( 1.e0 - wl2 )
        wr(i,j) = wd(i,j) - wl(i,j)
      end if
    end if
  end do
  end do
end if

!write(6,*) 'After', wr(19,14) , wl(19,14) , wd(14,19)
return
end subroutine urbanWL
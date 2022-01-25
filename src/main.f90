program calc_stress
  use ctrl_vars
  use utils, only: &
    & calc_Ademand, make_getgrid, make_urbanWL, comp_reverse, &
    & comp_jump, urbanWL, path_1g, get_monday, get_term, writer
    ! & calc_DIdemand, &
  implicit none
  real cwd(mx,my,2) !water stress , 1:sum( min(wr,wd) )  2:sum(wd)
  real cwdy(mx,my,2) !annual CWD
  real ocean(mx,my)
  real crpstg(mx,my,36) !when cropstg = 1,3,4 require water 12mm/day
  real win(mx,my)
  real outflw(mx,my)
  real catchment(mx,my) !catchment (consider jump)
  integer i, j, t
  integer irec
  integer iy , id , days
  character(4) cyear
  !----------------------------------------------------------------------

  ! --- Import Mask data
  open(11,file=trim(land_path)//'Mask_'//trim(suffix)//'.bin', &
    & form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  read(11,rec=1) ((mask(i,j),i=1,mx),j=1,my)
  close(11)

  ! --- Import Area data
  open(12,file=trim(land_path)//'AREA_'//trim(suffix)//'.bin',&
  &form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  read(12,rec=1) ((area(i,j),i=1,mx),j=1,my)
  close(12)
  ! for areadata, convert km^2 -> m^2
  do j = 1 , my
    do i = 1 , mx
      area(i,j) = area(i,j) * 1.e6
    end do
  end do

  ! --- Import ocean and paddy land mask data.
  open(13,file=trim(land_path)//'ClassFrac_'//trim(suffix)//'_MIRCA-GLCC.bin', &
    & form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  read(13,rec=2)  ((ocean(i,j),i=1,mx),j=1,my)
  read(13,rec=15) ((paddy(i,j),i=1,mx),j=1,my)
  close(13)

  ! --- Import import CamaFlood.next_xy data
  ! - => jx, jy
  open(14,file=trim(map_path)//'nextxy.bin', &
    & form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  read(14,rec=1) ((jx(i,j),i=1,mx),j=my,1,-1)
  read(14,rec=2) ((jy(i,j),i=1,mx),j=my,1,-1) !Then, contents of JY are South to North
  close(14)

  ! --- Import river rank data
  open(15,file=trim(map_path)//'rank.bin', &
    & form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  read(15,rec=1) ((rank(i,j,1),i=1,mx),j=my,1,-1)
  read(15,rec=2) ((rank(i,j,2),i=1,mx),j=my,1,-1)
  read(15,rec=3) ((rank(i,j,3),i=1,mx),j=my,1,-1)
  close(15)

  ! --- Import cathment area data
  open(16,file=trim(map_path)//'catchment-area.bin', &
    & form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  read(16,rec=1) ((catchment(i,j),i=1,mx),j=my,1,-1)
  close(16)
  do j = 1 , my
    do i = 1 , mx
      catchment2(i,j) = catchment(i,j)
    end do
  end do
  if( bjump ) call comp_jump(catchment)

  open(17,file=trim(land_path)//'cropstage_'//trim(suffix)//'.bin', &
    & form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
  do t = 1 , 36
    read(17,rec=t)  ((crpstg(i,j,t),i=1,mx),j=1,my)
  end do
  close(17)

  ! call calc_DIdemand !domestic and industrial demand
  wd_DI = 0

  call calc_Ademand

  if( b1gpath )  call make_getgrid
    iuwl(:,:) = 0 !No Urban Water Linw
  if( burbanwl ) call make_urbanWL

  cwd(:,:,:) = 0.e0    !Total CWD
  do iy = isy , iey
    cwdy(:,:,:) = 0.e0 !Annual CWD
    call get_monday(iy,days)
    write(cyear,'(i4)') iy
    open(21,file=trim(sibuc_path)//'SiBUC_'//trim(runname)//'_'//cyear//'_'//trim(suffix)//'.bin',&
      & form='unformatted', access='direct', recl=mx*my*ibin, status='old', action='read')
    open(22,file=trim(outflw_path)//'outflw'//cyear//'.bin',&
      & form='unformatted', access='direct', recl=mx*my*ibin, action='read', status='old')

  do id = 1 , days
    irec = (id-1) * 23 + 22
    read(21,rec=irec) ((win(i,j),i=1,mx),j=1,my)
    read(22,rec=id)   ((outflw(i,j),i=1,mx),j=my,1,-1)
    do j = 1 , my
    do i = 1 , mx
      if( outflw(i,j) > 1.e9 ) outflw(i,j) = 0.e0
    end do
    end do

    call comp_reverse(outflw)
    if( bjump ) call comp_jump(outflw)

    call get_term(iy,id,t)

    wl(:,:) = 0.e0
    do j = 1 , my
    do i = 1 , mx
      if( mask(i,j) < 0.5 ) cycle
      if( crpstg(i,j,t) == 1 .or. crpstg(i,j,t) == 3 .or. crpstg(i,j,t) == 4 .or. crpstg(i,j,t) == 5)then !Irrigation Term
        wd(i,j) = win(i,j) * 1.e-3 * area(i,j) * (1.e0-ocean(i,j)) / we + wd_DI(i,j) + wd_A(i,j) ![m3/day]
      else
        wd(i,j) = win(i,j) * 1.e-3 * area(i,j) * (1.e0-ocean(i,j)) / we + wd_DI(i,j) ![m3/day]
      end if

      wr(i,j) = max( (outflw(i,j) - catchment(i,j) * 3.e-9 ) * 86400.e0 &
        & + win(i,j) * 1.e-3 * area(i,j) * (1.e0 - ocean(i,j)) , 0.e0)
      wl(i,j) = wd(i,j) - wr(i,j)
    end do
    end do

    if( burbanwl ) call urbanWL(wd,wr,wl,outflw)
    if( b1gpath  ) call path_1g(wd,wr,wl)

    do j = 1 , my
    do i = 1 , mx
      cwd(i,j,1)  = cwd(i,j,1)  + min( wd(i,j) , wr(i,j) )
      cwdy(i,j,1) = cwdy(i,j,1) + min( wd(i,j) , wr(i,j) )
      cwd(i,j,2)  = cwd(i,j,2)  + wd(i,j)
      cwdy(i,j,2) = cwdy(i,j,2) + wd(i,j)
      if( wd(i,j) < 0.e0 .or. min(wd(i,j),wr(i,j)) < 0.e0 ) &
        & write(6,*) wd(i,j),min(wd(i,j),wr(i,j)),iy,id,i,j
    end do
    end do

  end do !id
  call writer(iy,cwdy)
  close(21) ; close(22)
end do !iy
call writer(9999,cwd)

write(6,*)'!!! CWD Calculation Finished !!!'

end program calc_stress

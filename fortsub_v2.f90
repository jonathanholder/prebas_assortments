! harvested assortment implementation in prebas
! jh, 2021-07-07

subroutine harvAssort1(spec, vol, n, h, age, sawnratio, enerratio, pulpratio)
implicit none
real (kind=8) :: spec, vol, n, h, age, sawnratio, enerratio, pulpratio

if(n == 0 ) then !n = 0 in first years of stand dev, won't be sensible to conduct mgmt, but might happen in forced 'real' mgmt applications
  sawnratio = 0 !--> only energy wood in regenerating stands
  enerratio = 1
else

 ! SPRUCE
if(spec == 1) then !1 = spruce; pine (below) for all else
    sawnratio = max(0., 0.95*(1-exp(1.2543-vol/n*1000*0.0044921-h*0.10810+h**2*0.0027574))) ! max to exclude negative values at low volumes
    enerratio = 0.01*(25 * 0.011141 * (1200 / (1000 * vol/n - 3.76) - 1) + 0.174)

  if(enerratio > 1 .OR. enerratio < 0) then !at low volumes, energy ratio can be negative or >1
      enerratio = 1 !--> set energy ratio to 1 in these cases
  endif

! PINE (and all else)
else if(spec /= 1) then
 sawnratio =  max(0., 0.96*(1-exp(0.42486-vol/n*1000*0.0051294-age*0.0037506))) ! low volumes, negative ratios...
 enerratio = min(1., 0.01*(21.838 * 0.014317 * (590.559 / ((vol/n * 1000 - 9.8)**0.9) - 1) + 0.174)) !low volumes, ratios >1...
endif
endif

pulpratio = 1 - sawnratio - enerratio
end subroutine harvAssort1

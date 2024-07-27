module precision
	!Precision:
	!All real variables defaulted to double precision
	integer, parameter	 :: realkind = selected_real_kind(p=13,r=200)
end module precision
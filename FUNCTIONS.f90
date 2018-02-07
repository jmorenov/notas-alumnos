subroutine calcular_soluciones(file, unit, N, CANSWER)
   implicit none
   
   character (len=*), intent(in) :: file
   integer, intent(in) :: unit, N
   real, intent(out) :: CANSWER(N)

   real :: num
   integer :: i

   open (UNIT=unit,FILE=file,STATUS='old', FORM='formatted')

   do i=1, N
      read (UNIT=unit, FMT='(E11.6)' ) num
      CANSWER(i) = num 
   end do

   close(UNIT=unit)
end subroutine calcular_soluciones

subroutine calcular_nota(CANSWER, line, N, CNOTA)
   implicit none
   
   real, intent(in) :: CANSWER(N)
   character (len=500), intent(in) :: line
   integer, intent(in) :: N
   character (len=2), intent(out) :: CNOTA

   integer :: NOTA, i, j
   real :: ANSWER(N)
   character (len=7) :: mantisa(N)
   integer :: nmantisa(N)
   character (len=3) :: exponente(N)
   integer :: nexponente(N)

   NOTA = 0
   CNOTA = ''
   j = 40

   do i=1, N
      mantisa(i) = line(j+1:j+7) !RESPUESTA1
      mantisa(i) = trim(mantisa(i)) // '0000000'
      read(mantisa(i),'(I7)') nmantisa(i)
      
      exponente(i) = line(j+8:j+10)
      read(exponente(i),'(I3)') nexponente(i)
         
      ANSWER(i) = real(nmantisa(i))*(10**real(nexponente(i)))/(10**5)
         
      j=j+10
      
      if(abs((ANSWER(i) - CANSWER(i))/CANSWER(i)) < 10.**(-4.) ) then
         NOTA = NOTA + 1 
      else 
         NOTA = NOTA
      end if
   end do

   write(CNOTA, '(I2)') NOTA

end subroutine calcular_nota

subroutine obtener_dni(line, DNI)
   implicit none
   
   character (len=500), intent(out) :: line
   character (len=8), intent(out) :: DNI

   integer :: i, j, K !ENTERO PARA CONTAR LAS ';'
   character (len=1) :: pyc !PUNTO Y COMA

   K=0 !CONTADOR DE ';'
   i=1 !EL VALOR FINAL DE 'i' A LA SALIDA DEL PRIMER BUCLE SERÁ LA POSICIÓN DE LA 3º ';' EN LA LINEA
        
   do while(K<=2) 
      pyc = line(i:i)

      if(pyc == ';') then
         K=K+1
      end if

   i=i+1
end do
        
   j=i !ENCADENAMOS EL ÚLTIMO VALOR DE 'i' CON EL PRIMERO DE 'j' PARA QUE CONTINUE EN LA MISMA PARTE DE LA LINEA
   do while(K<=3)
      pyc = line(j:j)
    
      if(pyc == ';') then
         K=K+1
        
         if(k==3) then
           exit
         endif
            
        endif
    
        j=j+1
   end do
        
   j=j-2
   DNI = line(i:j)
end subroutine obtener_dni
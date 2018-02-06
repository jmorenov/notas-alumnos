function calcular_soluciones(file, unit, N) result(CANSWER)
   implicit none
   
   character (len=100), intent(in) :: file
   integer, intent(in) :: unit, N

   real :: CANSWER(N)
   real :: num
   integer :: i

   open (UNIT=unit,FILE=file,STATUS='old', FORM='formatted')

   do i=1, N
      read (UNIT=unit, FMT='(E11.6)' ) num
      CANSWER(i) = num 
   end do
end function

function calcular_nota(CANSWER, line, N) result(CNOTA)
   implicit none
   
   real, intent(in) :: CANSWER(N)
   character (len=500), intent(in) :: line
   integer, intent(in) :: N

   character (len=2) :: CNOTA
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

end function

Program MARKS
   
   implicit none
   
   ! VARIABLES DE GUERRA
   
   integer :: i,j !INDEX
   integer :: EoF,FoE !IOSTAT
   integer, parameter :: N = 10
   real :: num

   character (len=500) :: line
   
   ! DNI
   character (len=8) :: DNI !DNI caracteres
   character (len=8) :: DNI2 !DNI numero
   
   ! BUCLE RESPUESTAS - NOTA
   real :: CANSWER(N)
   character (len=2) :: CNOTA
   character (len=2) :: calcular_nota
   real, dimension(N) :: calcular_soluciones
   
   ! BUCLE 2º DNI
   integer :: K !ENTERO PARA CONTAR LAS ';'
   character (len=1) :: pyc !PUNTO Y COMA
   character (len=500) :: line2 !line archivo 2
   
   character (len=8) :: DNI3
   character (len=8) :: DNI4
   
   ! APERTURA DE ARCHIVOS UTILIZABLES
   open (UNIT=1,FILE="p1.TXT",STATUS="old")
   open (UNIT=4,FILE="NOTAP1.csv",STATUS="new")
   open (UNIT=5,FILE="NOTASPERDIDAS.csv",STATUS="new")
  
   ! RESPUESTAS CORRECTAS
   CANSWER = calcular_soluciones('solutions.csv', 3, N)
   
   ! BUCLE PRINCIPAL
   EoF = 0
   do 

      if(EoF < 0) then 
         exit
      endif
      
      read (UNIT=1, IOSTAT=EoF, FMT='(A)') line

      ! DNI
      
      DNI = line(1:8)
      read(DNI,'(A)') DNI2
      
      ! COMPARACION Y EVALUACIÓN DEL ALUMNO
      CNOTA = calcular_nota(CANSWER, line, N) 
      
      ! ASIGNACIÓN DE LA NOTA A LOS ALUMNOS
      
         !LECTURA ARCHIVO GRUPOS.CSV
         
      open (UNIT=2,FILE="GRUPOS.csv",STATUS="old")
      read (UNIT=2, IOSTAT=FoE, FMT='(A)') line2
      
      do
         read (UNIT=2, IOSTAT=FoE, FMT='(A)') line2
         
         if(FoE < 0) then
         
            write(UNIT=5,FMT='(A)') DNI // ';' // CNOTA
            
            close(UNIT=2)
             
            exit
            
         endif
         
            K=0 !CONTADOR DE ';'
            i=1 !EL VALOR FINAL DE 'i' A LA SALIDA DEL PRIMER BUCLE SERÁ LA POSICIÓN DE LA 3º ';' EN LA LINEA
         
            do while(K<=2)
         
               pyc = line2(i:i)
         
               if(pyc == ';') then
                  K=K+1
               end if
            
               i=i+1
            end do
         
            j=i !ENCADENAMOS EL ÚLTIMO VALOR DE 'i' CON EL PRIMERO DE 'j' PARA QUE CONTINUE EN LA MISMA PARTE DE LA LINEA
         
            do while(K<=3)
         
               pyc = line2(j:j)
         
               if(pyc == ';') then
                  K=K+1
                  
                  if(k==3) then
                     exit
                  endif
                  
               endif
            
               j=j+1
            end do
             
            j=j-2
         
            DNI3 = line2(i:j)
            
            ! HAY QUE HACER UNA CORRECCIÓN DE ERRORES PARA LAS LETRAS INICIALES
            ! print *, DNI3;
            read(DNI3,'(A)') DNI4
            
            if(DNI2 == DNI4) then
            
               write(UNIT=4, FMT='(A)') line2 // ';' // CNOTA
               
               close(UNIT=2)
               
               exit
               
            endif
            
      end do

   end do
   
end program
C     Last change:  GES  14 Jun 2002    1:05 pm
      subroutine tnode_a (ndef,ftnode,iftran,inc,carry,depvar,e)
        dll_export tnode_a
        INTEGER, INTENT(IN) :: ndef(4)
        INTEGER, INTENT(IN) :: ftnode(ndef(1),2)
        INTEGER(2), INTENT(IN) :: iftran(ndef(1))
        INTEGER :: i,k,i_e
        DOUBLE PRECISION, INTENT(IN) :: inc(ndef(1),ndef(3)),
     1   carry(ndef(1),ndef(3)),depvar(ndef(1),ndef(3))
        DOUBLE PRECISION, INTENT(OUT) :: e(ndef(4))
        DOUBLE PRECISION :: node(ndef(2),ndef(3)),rch
        i_e=1
        do i=1,ndef(2)
          do k=1,ndef(3)
            node(i,k)=0
          end do
        end do
        do i=1,ndef(1)
          do k=1,ndef(3)
            rch=inc(i,k)+carry(i,k)*node(ftnode(i,1),k)
            if (depvar(i,k).gt.0) then
              e(i_e)=LOG(depvar(i,k)/rch)
              rch=depvar(i,k)
              i_e=i_e+1
            end if
            node(ftnode(i,2),k)=node(ftnode(i,2),k)+iftran(i)*rch
          end do
        end do
      end subroutine tnode_a

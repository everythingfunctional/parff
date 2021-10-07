module parff_position_m
    implicit none
    private
    public :: position_t

    type :: position_t
        integer :: line = 1
        integer :: column = 1
    contains
        private
        procedure, public :: next_position
    end type
contains
    pure function next_position(self, char_)
        class(position_t), intent(in) :: self
        character(len=1), intent(in) :: char_
        type(position_t) :: next_position

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE_ = char(10)

        if (char_ == NEWLINE_) then
            next_position%line = self%line + 1
            next_position%column = 1
        else if (char_ == TAB) then
            next_position%line = self%line
            next_position%column = self%column + 8 - mod(self%column - 1, 8)
        else
            next_position%line = self%line
            next_position%column = self%column + 1
        end if
    end function
end module

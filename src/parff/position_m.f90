module parff_position_m
    implicit none
    private
    public :: position_t, new_position

    type :: position_t
        private
        integer :: line_
        integer :: column_
    contains
        private
        procedure, public :: line
        procedure, public :: column
        procedure, public :: next_position
    end type
contains
    pure function new_position()
        type(position_t) :: new_position

        new_position%line_ = 1
        new_position%column_ = 1
    end function

    pure function line(self)
        class(position_t), intent(in) :: self
        integer :: line

        line = self%line_
    end function

    pure function column(self)
        class(position_t), intent(in) :: self
        integer :: column

        column = self%column_
    end function

    pure function next_position(self, char_)
        class(position_t), intent(in) :: self
        character(len=1), intent(in) :: char_
        type(position_t) :: next_position

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE_ = char(10)

        if (char_ == NEWLINE_) then
            next_position%line_ = self%line_ + 1
            next_position%column_ = self%column_
        else if (char_ == TAB) then
            next_position%line_ = self%line_
            next_position%column_ = self%column_ + 8 - mod(self%column_ - 1, 8)
        else
            next_position%line_ = self%line_
            next_position%column_ = self%column_ + 1
        end if
    end function
end module

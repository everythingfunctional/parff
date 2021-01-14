module parff_parsed_character_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_character_t

    type, extends(parsed_value_t) :: parsed_character_t
        private
        character(len=1) :: value__
    contains
        private
        procedure, public :: value_
    end type

    interface parsed_character_t
        module procedure constructor
    end interface
contains
    pure function constructor(value_) result(parsed_character)
        character(len=1), intent(in) :: value_
        type(parsed_character_t) :: parsed_character

        parsed_character%value__ = value_
    end function

    pure function value_(self)
        class(parsed_character_t), intent(in) :: self
        character(len=1) :: value_

        value_ = self%value__
    end function
end module

module parff_parsed_integer_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_integer_t

    type, extends(parsed_value_t) :: parsed_integer_t
        private
        integer :: value__
    contains
        private
        procedure, public :: value_
    end type

    interface parsed_integer_t
        module procedure constructor
    end interface
contains
    pure function constructor(value_) result(parsed_integer)
        integer, intent(in) :: value_
        type(parsed_integer_t) :: parsed_integer

        parsed_integer%value__ = value_
    end function

    pure function value_(self)
        class(parsed_integer_t), intent(in) :: self
        integer :: value_

        value_ = self%value__
    end function
end module

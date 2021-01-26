module parff_parsed_rational_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_rational_t

    type, extends(parsed_value_t) :: parsed_rational_t
        private
        double precision :: value__
    contains
        private
        procedure, public :: value_
    end type

    interface parsed_rational_t
        module procedure constructor
    end interface
contains
    pure function constructor(value_) result(parsed_rational)
        double precision, intent(in) :: value_
        type(parsed_rational_t) :: parsed_rational

        parsed_rational%value__ = value_
    end function

    pure function value_(self)
        class(parsed_rational_t), intent(in) :: self
        double precision :: value_

        value_ = self%value__
    end function
end module

module parff_parsed_string_m
    use iso_varying_string, only: varying_string, assignment(=)
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_string_t

    type, extends(parsed_value_t) :: parsed_string_t
        type(varying_string) :: value_
    end type

    interface parsed_string_t
        module procedure constructor_c
        module procedure constructor_s
    end interface
contains
    pure function constructor_c(value_) result(parsed_string)
        character(len=*), intent(in) :: value_
        type(parsed_string_t) :: parsed_string

        parsed_string%value_ = value_
    end function

    pure function constructor_s(value_) result(parsed_string)
        type(varying_string), intent(in) :: value_
        type(parsed_string_t) :: parsed_string

        parsed_string%value_ = value_
    end function
end module

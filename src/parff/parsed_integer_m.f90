module parff_parsed_integer_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_integer_t

    type, extends(parsed_value_t) :: parsed_integer_t
        integer :: value_
    end type
end module

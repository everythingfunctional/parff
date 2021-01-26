module parff_parsed_nothing_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_nothing_t, PARSED_NOTHING

    type, extends(parsed_value_t) :: parsed_nothing_t
    end type

    type(parsed_nothing_t), parameter :: PARSED_NOTHING = parsed_nothing_t()
end module

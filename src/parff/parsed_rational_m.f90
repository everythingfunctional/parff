module parff_parsed_rational_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_rational_t

    type, extends(parsed_value_t) :: parsed_rational_t
        double precision :: value_
    end type
end module

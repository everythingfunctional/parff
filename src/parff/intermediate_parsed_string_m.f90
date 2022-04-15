module parff_intermediate_parsed_string_m
    use iso_varying_string, only: varying_string, assignment(=)
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: intermediate_parsed_string_t

    type, extends(parsed_value_t) :: intermediate_parsed_string_t
        type(varying_string) :: parsed_so_far
        type(varying_string) :: left_to_parse
    end type

    interface intermediate_parsed_string_t
        module procedure constructor_cs
        module procedure constructor_ss
    end interface
contains
    pure function constructor_cs( &
            parsed_so_far, left_to_parse) result(intermediate_parsed_string)
        character(len=*), intent(in) :: parsed_so_far
        type(varying_string), intent(in) :: left_to_parse
        type(intermediate_parsed_string_t) :: intermediate_parsed_string

        intermediate_parsed_string%parsed_so_far = parsed_so_far
        intermediate_parsed_string%left_to_parse = left_to_parse
    end function

    pure function constructor_ss( &
            parsed_so_far, left_to_parse) result(intermediate_parsed_string)
        type(varying_string), intent(in) :: parsed_so_far
        type(varying_string), intent(in) :: left_to_parse
        type(intermediate_parsed_string_t) :: intermediate_parsed_string

        intermediate_parsed_string%parsed_so_far = parsed_so_far
        intermediate_parsed_string%left_to_parse = left_to_parse
    end function
end module

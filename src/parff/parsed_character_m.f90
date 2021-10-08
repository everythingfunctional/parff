module parff_parsed_character_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_character_t

    type, extends(parsed_value_t) :: parsed_character_t
        character(len=1) :: value_
    end type
end module

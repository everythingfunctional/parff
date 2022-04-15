module parff_intermediate_repeat_m
    use parff_parsed_items_m, only: parsed_items_t
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: intermediate_repeat_t

    type, extends(parsed_value_t) :: intermediate_repeat_t
        type(parsed_items_t) :: parsed_so_far
        integer :: remaining
    end type
end module

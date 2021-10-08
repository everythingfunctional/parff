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

    interface intermediate_repeat_t
        module procedure constructor
    end interface
contains
    function constructor(parsed_so_far, remaining) result(intermediate_repeat)
        type(parsed_items_t), intent(in) :: parsed_so_far
        integer, intent(in) :: remaining
        type(intermediate_repeat_t) :: intermediate_repeat

        intermediate_repeat%parsed_so_far = parsed_so_far
        intermediate_repeat%remaining = remaining
    end function
end module

module parff_parsed_item_m
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_item_t

    type :: parsed_item_t
        class(parsed_value_t), allocatable :: item
    end type

    interface parsed_item_t
        module procedure constructor
    end interface
contains
    function constructor(item) result(parsed_item)
        class(parsed_value_t), intent(in) :: item
        type(parsed_item_t) :: parsed_item

        allocate(parsed_item%item, source = item)
    end function
end module

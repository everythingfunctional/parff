module parff_parsed_item_list_m
    use parff_parsed_item_m, only: parsed_item_t
    use parff_parsed_value_m, only: parsed_value_t

    implicit none
    private
    public :: parsed_item_list_t

    type :: parsed_item_node_t
        class(parsed_value_t), allocatable :: item
        type(parsed_item_node_t), pointer :: next => null()
    end type

    type :: parsed_item_list_t
        private
        type(parsed_item_node_t), pointer :: head => null()
        type(parsed_item_node_t), pointer :: tail => null()
        integer :: size
    contains
        procedure :: append
        procedure :: to_array
    end type

    interface parsed_item_list_t
        module procedure with_first_item
    end interface
contains
    function with_first_item(item) result(list)
        class(parsed_value_t), intent(in) :: item
        type(parsed_item_list_t) :: list

        allocate(list%head)
        list%tail => list%head
        list%size = 1
        list%head%item = item
    end function

    subroutine append(self, item)
        class(parsed_item_list_t), intent(inout) :: self
        class(parsed_value_t), intent(in) :: item

        allocate(self%tail%next)
        self%tail => self%tail%next
        self%tail%item = item
        self%size = self%size + 1
    end subroutine

    function to_array(self) result(array)
        class(parsed_item_list_t), intent(inout) :: self
        type(parsed_item_t), allocatable :: array(:)

        type(parsed_item_node_t), pointer :: curr
        integer :: i

        allocate(array(self%size))
        curr => self%head
        do i = 1, self%size
            call move_alloc(curr%item, array(i)%item)
            curr => curr%next
            deallocate(self%head)
            self%head => curr
        end do
    end function
end module

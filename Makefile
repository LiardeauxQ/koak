##
## Makefile
## alexandre.fourcat@epitech.eu
## 2019
##

ROOT	=	.

NAME	=	koak

CC		=	stack

V		=	@

all: $(NAME)

$(NAME):
	$(V)$(CC) build
	$(V)cp `$(CC) path --local-install-root`/bin/$@-exe $@

clean:
	$(V) $(CC) clean
	$(V)$(CC) clean

fclean: clean
	$(V)rm -rf $(NAME)

re: fclean all

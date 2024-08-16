NAME = readySetBool
CMP = ghc
MAIN = main.hs
SRC = adder.hs boolean_evaluation.hs gray_code.hs multiplier.hs sub.hs truth_table.hs
FLAGS = -no-keep-hi-files -no-keep-o-files

all: $(NAME)

$(NAME): $(SRC)
	$(CMP) $(FLAGS) $(MAIN) --make $(SRC) -o $(NAME)

run: $(NAME)
	@echo "------------------------------------------------"
	@echo "---               RUNNING                    ---"
	@echo "------------------------------------------------"
	./$(NAME)

clean:
	rm $(NAME)

.Phony: run all
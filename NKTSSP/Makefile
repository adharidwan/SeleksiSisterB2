# File: NKTSSP/Makefile (Final Corrected Version)

# Nama executable yang akan dihasilkan
TARGET = nktssrv

# Compiler dan Assembler
CC = gcc
AS = nasm

# File source
ASM_SRC = main.asm
C_SRC = auth.c

# File object
OBJS = $(ASM_SRC:.asm=.o) $(C_SRC:.c=.o)

# Flags
ASFLAGS = -f elf64
CFLAGS = -c -fPIC -Wall
LDFLAGS = -no-pie # <-- CHANGE: Tambahkan flag ini kembali

# Target utama
all: $(TARGET)

# Aturan untuk linking
$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $(TARGET) $(OBJS)
	@echo "✅ Server executable '$(TARGET)' created successfully."

# Aturan untuk mengompilasi C
%.o: %.c
	$(CC) $(CFLAGS) -o $@ $<

# Aturan untuk merakit Assembly
%.o: %.asm
	$(AS) $(ASFLAGS) $< -o $@

# Aturan untuk membersihkan direktori
clean:
	rm -f *.o $(TARGET)
	@echo "🧹 Cleanup complete."

.PHONY: all clean
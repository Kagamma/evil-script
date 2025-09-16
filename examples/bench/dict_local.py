import time

def calculate_product_sum():
    # Initialize total
    total = {'x': 0}

    # Get start time in milliseconds
    t = time.time() * 1000

    # Nested loops
    for i in range(10000):
        for j in range(10000):
            total['x'] = total['x'] + i * j

    # Calculate elapsed time
    elapsed_time = (time.time() * 1000) - t

    # Return results
    return total['x'], elapsed_time

# Call the function and print results
result, time_taken = calculate_product_sum()
print(f"\nTime: {time_taken:.2f} ms")
print(f"Total: {result}")
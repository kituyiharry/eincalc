<script>
  import { Canvas, Layer } from 'svelte-canvas';

  const CELL_WIDTH       = 120;
  const CELL_HEIGHT      = 30;
  const HEADER_HEIGHT    = 30;
  const ROW_HEADER_WIDTH = 60;

  let scrollX = 0;
  let scrollY = 0;

  function getColumnLabel(col) {
    let label = [];
    let num   = col;
    while (num >= 0) {
      label.push(String.fromCharCode(65 + (num % 26)));
      num = Math.floor(num / 26) - 1;
    }
    return label.join('');
  }

</script>

<div class='h-[100vh] w-[100vw]'>
    <!-- Watch out for  -->
    <Canvas>
        <Layer render={({ context, width, height }) => { 

            // Calculate visible range
            const startCol = Math.floor(scrollX / CELL_WIDTH);
            const endCol   = Math.ceil((scrollX + width) / CELL_WIDTH);
            const startRow = Math.floor(scrollY / CELL_HEIGHT);
            const endRow   = Math.ceil((scrollY + height) / CELL_HEIGHT);

            // Draw cells
            context.strokeStyle = '#ddd';
            context.fillStyle   = '#000';
            context.font        = '12px sans-serif';

            for (let row = startRow; row <= endRow; row++) {
              for (let col = startCol; col <= endCol; col++) {
                const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;

                // Call visibility callback
                //onCellVisible(row, col);

                // Highlight selected cells
                // if (isCellSelected(row, col)) {
                //   ctx.fillStyle = '#e3f2fd';
                //   ctx.fillRect(x, y, CELL_WIDTH, CELL_HEIGHT);
                //   ctx.fillStyle = '#000';
                // }

                // Draw cell border
                context.strokeRect(x, y, CELL_WIDTH, CELL_HEIGHT);

                // Draw cell content
                const cellKey = `${row},${col}`;
                // const cellValue = cellData[cellKey] || '';
                // if (cellValue) {
                    context.fillStyle = '#000';
                    context.fillText(cellKey, x + 5, y + 20);
                // }
              }
            }


            // Draw column headers
            context.fillStyle = '#f5f5f5';
            context.fillRect(ROW_HEADER_WIDTH, 0, width - ROW_HEADER_WIDTH, HEADER_HEIGHT);
            context.fillStyle = '#000';
            context.font = 'bold 12px sans-serif';
            context.strokeStyle = '#ddd';

            for (let col = startCol; col <= endCol; col++) {
              const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
              
              // Highlight selected column headers
              // if (bounds && col >= bounds.startCol && col <= bounds.endCol) {
              //   context.fillStyle = '#cce5ff';
              //   context.fillRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
              //   context.fillStyle = '#000';
              // }
              
              context.strokeRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
              context.fillText(getColumnLabel(col), x + 5, 20);
            }



            // Draw row headers
            context.fillStyle = '#f5f5f5';
            context.fillRect(0, HEADER_HEIGHT, ROW_HEADER_WIDTH, height - HEADER_HEIGHT);
            context.fillStyle = '#000';

            for (let row = startRow; row <= endRow; row++) {
              const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;
              
              // Highlight selected row headers
              // if (bounds && row >= bounds.startRow && row <= bounds.endRow) {
              //   context.fillStyle = '#cce5ff';
              //   context.fillRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
              //   context.fillStyle = '#000';
              // }
              
              context.strokeRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
              context.fillText(`${(row + 1)}`, 5, y + 20);
            }

        }} />
    </Canvas>
</div>
